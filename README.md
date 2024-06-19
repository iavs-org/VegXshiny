
<!-- README.md is generated from README.Rmd. Please edit that file -->

# VegXshiny

**Veg-X is a standard for the exchange of vegetation data. VegXshiny is
an interactive web application for converting datasets into Veg-X and
back.**

## Background

The increasing digitization of research data has motivated data
standardization efforts across scientific disciplines. For vegetation
data, Veg-X (Wiser et al., 2011) is such a standard. It provides both
flexibility and precision in representing vegetation data of different
origins and formats. It’s scope is the exchange of data-sets both
between vegetation scientists and between vegetation scientists and
database operators.

VegXshiny was developed as a GUI-based application for creating Veg-X
documents and is build around the R package VegX (De Cáceres, 2018). It
also allows the conversion from Veg-X documents to other formats.

The figure below shows a typical workflow. Start with the ‘Start’ tab of
the main menu. The second step is import, where dialogs guide the user
through the process of mapping data to the appropriate Veg-X elements.
Once these steps have been completed, the VegX code can be reviewed and
downloaded.

<left> <img src="inst/app/www/images/Flowchart.svg" width="580" />

The ‘Start’, ‘Import’, ‘Review’, ‘Download’ and ‘Action Log’ tabs have
their own help sections, which describe their functionality in detail.

<div style="text-align:left">

<a href="http://37.120.167.83" target="_blank"> Online version of this
app </a>

</div>

------------------------------------------------------------------------

**Package development**

Christian König, Sebastian Schmidtlein

**Acknowledgments**

The development of VegXshiny is endorsed by the International
Association for Vegetation Science (IAVS) and received funding by the
German Research Foundation (DFG, project number 460840087).

Development of the R package VegX: Miquel De Cáceres

Veg-X standard development: Brad Boyle, Miquel De Cáceres, Martin
Kleikamp, Christian König, Robert K. Peet, Sebastian Schmidtlein, Nick
Spencer, Susan K. Wiser

**References**

Wiser, S.K., Spencer, N., de Cáceres, M., Kleikamp, M., Boyle, B., Peet
R.K. (2011): Veg-X - an exchange standard for plot-based vegetation
data. Journal of Vegetation Science 22: 598-609,
<doi:10.1111/j.1654-1103.2010.01245.x> .

De Cáceres (2018): VegX: Vegetation data in Veg-X. R-package,
<https://iavs-org.github.io/VegX>
