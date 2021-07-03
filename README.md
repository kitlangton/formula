# 🧪 Formula

[![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases]
[![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots]

Derive functional, reactive, Laminar forms at compile-time with this type-safe, composable **form combinator library**.

```sbt
// build.sbt
libraryDependencies += "io.github.kitlangton" %%% "formula" % "x.y.z"
```

## Resources

This project originally started as an example of using Magnolia with Scala.js. While the internals has shifted substantially since the recording, the original video is still available, [Deriving the Frontend](https://youtu.be/JHriftPO62I).

## Running Locally

1. `sbt ~fastLinkJS` in another tab.
2. `yarn install`
3. `yarn exec vite`
4. open `http://localhost:3000`

[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/io.github.kitlangton/formula_sjs1_2.13.svg "Sonatype Releases"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/io.github.kitlangton/formula_sjs1_2.13.svg "Sonatype Snapshots"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/io/github/kitlangton/formula_sjs1_2.13/ "Sonatype Snapshots"
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/io/github/kitlangton/formula_sjs1_2.13/ "Sonatype Releases"

