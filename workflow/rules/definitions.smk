def regex_choice_list(choices):
    choices = "|".join([str(x) for x in set(choices)])

    return f"(?:{choices})"


response = [x["id"] for x in config["model"]["response"]]
model_scope = [x["id"] for x in config["model"]["scope"]]

grouping = [x["id"] for x in config["variance partitioning"]["grouping"]]
partition_scope = [x["id"] for x in config["variance partitioning"]["scope"]]
partition = [x["id"] for x in config["variance partitioning"]["partition"]]
measure = [x["id"] for x in config["variance partitioning"]["measure"]]

conditions = [
    "patch",
    "year",
    "network",
    "viable",
    "survey_area",
    "vegetation",
    "core_area",
    "sample",
]


wildcard_constraints:
    response=regex_choice_list(response),
    model_scope=regex_choice_list(model_scope),
    grouping=regex_choice_list(grouping),
    partition_scope=regex_choice_list(partition_scope),
    partition=regex_choice_list(partition),
    measure=regex_choice_list(measure),
    condition=regex_choice_list(conditions),
