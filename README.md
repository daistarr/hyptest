
<img src="https://github.com/harvard-stat108s23/project2-group20/blob/199dab03d4b60f23e5d64293976a9e2cfaf5b4ea/hyptest.png" width="10%" height="10%">

# hyptest

Hyptest is a R-package that intends to facilitate the use of common randomization-based statistical
inference approaches and hypothesis testing.

---

#### Why should I use this package?
If you ever wondered:
What is Chi-Square test? What is the difference between t-test and z-test? What is null hypothesis? 
... and other juicy statistical questions, this package is for you. The idea behind this package is to help people to explore the power (and beauty) of using statistics.

---

#### How do I access this package?

STEP 1. Install and use the devtools package

```
install.packages("devtools")
library(devtools)
```

STEP 2. Install hyptest

```
install_github("harvard-stat108s23/project2-group20/hyptest")
```

STEP 3. Load the hyptest package

```
library(hyptest)
```
---

#### How do I use this package?
A. Please start in the vignette! It has a function that will help you to shortlist what is the best common statistical inference approach and hypotesis testing for your data.

B. Right now you can explore some test statistics using the functions below:
* est_chi: chi-square
* est_corr: correlation coefficient
* est_diff_means: difference in means
* est_diff_props: difference in proportions
* est_mean: one-sample mean
* est_prop: one-sample proportion

C. And you can use hypothesis testing:
* hyp_mean

D. More functions are coming soon! 

---
### Example

#### est_mean

In this example, the code provides the sample mean of the [palmerpenguins'](https://allisonhorst.github.io/palmerpenguins/) bill lenght (statistic) from a bootstrap distribution of 1000 samples. 

*Required packages*
```
library(hyptest)
library(palmerpenguins)
```

*Function to compute the inference for a population mean, where its graph will be exported as pdf*
```
est_mean(penguins, bill_length_mm, cil= 0.99, save_as = "pdf")
```

*Output*

<img src="https://github.com/harvard-stat108s23/project2-group20/blob/d93d66f3c510cd0039687001e44252e1103141f8/hyptest/man/figures/mean.png" width="70%" height="70%">

```
"[1] The sample mean is 43.99"
"[1] The true population mean is between 43.22 and 44.77 at 99% confidence interval"
```

<br>
<br>
<a href="mailto:daiannestarr@gmail.com">
<img src="https://github.com/edent/SuperTinyIcons/blob/df4f6767394eb2cbfa11330bcd0ddecac9a0d42b/images/svg/gmail.svg" width="4%" height="44%"></a>&nbsp;

<a href="http://www.linkedin.com/in/dfstarr"> 
<img src="https://github.com/edent/SuperTinyIcons/blob/df4f6767394eb2cbfa11330bcd0ddecac9a0d42b/images/svg/linkedin.svg" width="4%" height="4%"></a>&nbsp;

<a href="http://www.github.com/daistarr"> 
<img src="https://github.com/edent/SuperTinyIcons/blob/df4f6767394eb2cbfa11330bcd0ddecac9a0d42b/images/svg/github.svg" width="4%" height="4%"></a>&nbsp;

<a href="https://twitter.com/DaianneStarr"> 
<img src="https://github.com/edent/SuperTinyIcons/blob/df4f6767394eb2cbfa11330bcd0ddecac9a0d42b/images/svg/twitter.svg" width="4%" height="4%"></a>


