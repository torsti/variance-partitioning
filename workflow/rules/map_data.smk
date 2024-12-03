from os.path import basename, dirname


def orthographic_projection(lat_0, lon_0, earth_radius):
    return f"+proj=ortho +lat_0={lat_0} +lon_0={lon_0} +x_0=0 +y_0=0 +a={earth_radius} +b={earth_radius} +units=m +no_defs"


def mean_earth_radius(epsg):
    from osgeo.osr import SpatialReference

    srs = SpatialReference()
    srs.ImportFromEPSG(epsg)

    return (2 * srs.GetSemiMajor() + srs.GetSemiMinor()) / 3


rule download_gshhg:
    output:
        protected("results/data/map/gshhg-{format}-{version}.{archive}"),
    wildcard_constraints:
        format="(?:gmt|shp|bin)",
        version="2.3.7",
        archive="(?:zip|tar[.]gz)",
    params:
        url=lambda w, output: "http://www.soest.hawaii.edu/pwessel/gshhg/"
        + basename(output[0]),
    message:
        """Download GSHHG data version {wildcards.version} in {wildcards.format} format."""
    shell:
        "wget -O {output:q} {params.url:q}"


rule download_natural_earth_physical_data:
    output:
        protected(
            "results/data/map/ne/{resolution}/physical/ne_{resolution}_{layer}.zip"
        ),
    params:
        dir=lambda w, output: dirname(output[0]),
        url=lambda w: f"https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/{w.resolution}/physical/ne_{w.resolution}_{w.layer}.zip",
    wildcard_constraints:
        resolution="(?:10|50|110)m",
        layer="(?:land)",
    message:
        """Download Natural Earth physical layer "{wildcards.layer}" at {wildcards.resolution} resolution."""
    shell:
        "wget --directory-prefix={params.dir:q} {params.url:q}"


rule hemisphere:
    output:
        gml="results/data/map/sphere/{latitude}-{longitude}/hemisphere.gml",
        xsd="results/data/map/sphere/{latitude}-{longitude}/hemisphere.xsd",
    wildcard_constraints:
        latitude=r"\d+[.]\d+",
        longitude=r"\d+[.]\d+",
    params:
        epsg=4326,
        orthographic_projection=lambda w: orthographic_projection(
            w.latitude, w.longitude, mean_earth_radius(4326)
        ),
        n_points=10000,
        point_density=0.5,
    script:
        "../scripts/hemisphere.py"


rule sphere:
    input:
        hemisphere=rules.hemisphere.output.gml,
        natural_earth=rules.download_natural_earth_physical_data.output[0],
    output:
        gml="results/data/map/sphere/{latitude}-{longitude}/{resolution}/{category}/ne_{resolution}_{layer}.gml",
        xsd="results/data/map/sphere/{latitude}-{longitude}/{resolution}/{category}/ne_{resolution}_{layer}.xsd",
    params:
        orthographic_projection=lambda w: orthographic_projection(
            w.latitude, w.longitude, mean_earth_radius(4326)
        ),
    shell:
        "ogr2ogr"
        " -clipsrc {input.hemisphere:q}"
        " -f GML"
        " -t_srs {params.orthographic_projection:q}"
        " {output.gml:q}"
        " '/vsizip/{input.natural_earth}'"
        " --config OGR_ENABLE_PARTIAL_REPROJECTION TRUE"
        " -skipfailures"
