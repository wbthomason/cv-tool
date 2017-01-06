# cv-tool
A simple tool for easily building academic CVs in LaTeX from {YAML, Toml, JSON} and bibtex data

## About
`cv-tool` is designed to make it easy to record the information which should go in an academic CV
once in any of YAML, Toml, or JSON using a simple format and be able to render the corresponding CV
in LaTeX, PDF, JSON, Markdown, or HTML. The idea is that this makes it much more feasible to keep
one's CV up to date and consistent across a personal website, applications, and other tools.

## Status
`cv-tool` is "done", in that it works well for YAML specifications (and thus theoretically for Toml
and JSON), but hasn't been tested extensively and is in need of cleanup. The tasks remaining are
listed as issues on this repo. That said, `cv-tool` is definitely usable in its current state - I
use it to create and maintain my own CV currently. The only known missing feature is publication and
presentation parsing from Bibtex, but this should be done soon, and presentations and publications
can still be added in the manual format.

## Building
This should build with a reasonably up-to-date
[stack](https://docs.haskellstack.org/en/stable/README/). I may also add it to the AUR once it's a
bit more cleaned up and Bibtex support has been added.

## Use
Look at cv.yaml for an example of all of the CV elements supported and how to enter them. Put in
your details, and run `cv-tool cv.yaml` to generate (by default) a PDF of the CV. Run `cv-tool -h`
to see how to specify different input and output formats. To change the template used for the CV,
modify `default.<Filetype>`, where `<Filetype>` is the type of your desired output (`.tex` for the
case of PDF output, since we rely on LaTeX to make pretty PDFs)
