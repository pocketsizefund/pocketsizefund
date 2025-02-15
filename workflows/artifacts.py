from flytekit.core.artifact import Artifact, Granularity

RawBarsArtifact = Artifact(
    name="raw_daily_aggregated_bars",
    time_partitioned=True,
    time_partition_granularity=Granularity.DAY,
)

BarsArtifact = Artifact(
    name="daily_aggregated_bars", time_partitioned=True, time_partition_granularity=Granularity.DAY
)
