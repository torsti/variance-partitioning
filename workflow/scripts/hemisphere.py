from cmath import phase, rect
from math import copysign, pi, sin, cos, atan2
from numpy import linspace, array
from osgeo import ogr
from osgeo import osr

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from snakemake.script import Snakemake

    snakemake = Snakemake()  # type: ignore

output = snakemake.output.gml

epsg = snakemake.params.epsg
orthographic_projection = snakemake.params.orthographic_projection
n_points = snakemake.params.n_points
point_density = snakemake.params.point_density

srs_world = osr.SpatialReference()
srs_world.ImportFromEPSG(epsg)

srs_ortho = osr.SpatialReference()
srs_ortho.ImportFromProj4(orthographic_projection)

earth_radius = srs_ortho.GetSemiMajor()
latitude = srs_ortho.GetNormProjParm("latitude_of_origin")
longitude = srs_ortho.GetNormProjParm("central_meridian")

ortho_to_world = osr.CoordinateTransformation(srs_ortho, srs_world)
world_to_ortho = osr.CoordinateTransformation(srs_world, srs_ortho)

position = {
    "equator": latitude == 0,
    "antimeridian": abs(longitude) >= 90,
    "polar": abs(latitude) == 90
}

equator = 0
meridian = 0
antimeridian = 180

antimeridian_east =  antimeridian
antimeridian_west = -antimeridian

if (position["polar"]):
    pole = latitude

    circle_points = [[
        (antimeridian_west, pole),
        (antimeridian_west, equator),
        (antimeridian_east, equator),
        (antimeridian_east, pole),
        (antimeridian_west, pole)
    ]]

elif (position["equator"]):
    south = -90
    north =  90

    if (position["antimeridian"]):
        east = -antimeridian - copysign(antimeridian - abs(longitude), longitude) +  antimeridian / 2
        west =  antimeridian - copysign(antimeridian - abs(longitude), longitude) -  antimeridian / 2

        circle_points = [[
            (antimeridian_west, south),
            (antimeridian_west, north),
            (east, north),
            (east, south),
            (antimeridian_west, south)
        ],[
            (antimeridian_east, south),
            (antimeridian_east, north),
            (west, north),
            (west, south),
            (antimeridian_east, south)
        ]]

    else:
        west = longitude + antimeridian_west / 2
        east = longitude + antimeridian_east / 2

        circle_points = [[
            (west, south),
            (west, north),
            (east, north),
            (east, south),
            (west, south)
        ]]

else:
    splits = 4
    angles = linspace(0, 2 * pi, n_points)

    def angle_to_world(x, earth_radius = earth_radius):
        point = ortho_to_world.TransformPoint(
            cos(x) * earth_radius,
            sin(x) * earth_radius
        )

        return point[1::-1]

    def estimate_antimeridian_angle():
        from scipy.optimize import minimize

        def antimeridian_angle_diff(x):
            return antimeridian - angle_to_world(x[0])[0]

        res = minimize(
            antimeridian_angle_diff,
            array([0]),
            method = "nelder-mead",
            options = {"xatol": 1e-12}
        )

        return res.x

    antimeridian_angle = estimate_antimeridian_angle()
    antimeridian_latitude = angle_to_world(antimeridian_angle)[1]

    angles = angles + antimeridian_angle

    circle_points = [ angle_to_world(x) for x in angles ]

    pole = copysign(90, latitude)

    circle_points.sort(reverse = latitude > 0, key = lambda x: x[0])

    if (latitude > 0):
        circle_points.extend([
            (antimeridian_west, antimeridian_latitude),
            (antimeridian_west, pole),
            (antimeridian_east, pole),
            (antimeridian_east, antimeridian_latitude),
            circle_points[0]
        ])


    else:
        antimeridian_latitude  = circle_points[0][1] + (circle_points[0][1] - circle_points[1][1])

        circle_points.extend([
            (antimeridian_east, antimeridian_latitude),
            (antimeridian_east, pole),
            (antimeridian_west, pole),
            (antimeridian_west, antimeridian_latitude),
            circle_points[0]
        ])

    circle_points = [circle_points]

multipolygon = ogr.Geometry(ogr.wkbMultiPolygon)

for r in circle_points:
    ring = ogr.Geometry(ogr.wkbLinearRing)
    polygon = ogr.Geometry(ogr.wkbPolygon)

    for p in r:
        ring.AddPoint(p[0], p[1])

    ring.Segmentize(point_density)
    polygon.AddGeometry(ring)
    multipolygon.AddGeometry(polygon)

multipolygon.FlattenTo2D()

gml = ogr.GetDriverByName("GML")
output = gml.CreateDataSource(output)
layer = output.CreateLayer("bounds", srs_world, ogr.wkbMultiPolygon)

feature = ogr.Feature(layer.GetLayerDefn())
feature.SetGeometry(multipolygon)
layer.CreateFeature(feature)

del(output)
