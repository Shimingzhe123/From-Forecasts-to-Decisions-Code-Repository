# Emergency Department Forecasting and Staffing Optimization

**Research code for three connected PhD studies on demand forecasting, forecast evaluation, and staffing decisions in urgent and emergency care.**

This repository accompanies the PhD research of **Mingzhe Shi** at **Cardiff Business School, Cardiff University**, supervised by **Bahman Rostami-Tabar** and **Daniel Gartner**. The work examines a central question:

> **Does a more accurate forecast necessarily lead to a better staffing decision?**

The research shows why the answer is not always yes. Forecast error magnitude is important, but **bias, the timing of errors, demand uncertainty, staffing constraints, and recourse flexibility** can also affect operational performance. The repository is intended to make the research workflow and computational experiments easier to understand, reproduce, and extend.

**Funding:** Cardiff Business School Studentship.

> **Repository status:** This README describes the planned organization of the research code. File paths and commands shown as examples must be checked against the actual uploaded code before being treated as executable instructions. The research dataset is not included or promised to be publicly released.

## 1. Research overview

The PhD comprises three connected studies:

| Study | Focus | Central question | Main contribution |
| --- | --- | --- | --- |
| **Paper 1 — Systematic literature review** | Forecasting in unscheduled care | How has forecasting been studied and evaluated? | A structured review of **156 studies** and a framework connecting forecasting practice with planning and decision needs. |
| **Paper 2 — Point forecasting and deterministic staffing** | Forecast-to-decision evaluation | How does point forecast performance translate into staffing outcomes? | Demonstrates that conventional error metrics do not fully explain downstream staffing decisions and imbalances. |
| **Paper 3 — Probabilistic forecasting and stochastic staffing** | Uncertainty-aware decisions | Under what conditions is accounting explicitly for demand uncertainty valuable? | Evaluates probabilistic forecasts and two-stage stochastic optimization, including how operational flexibility affects the value of stochastic planning. |

The overall research sequence is:

```text
Evidence from forecasting literature
                |
                v
Point forecasts --> workload requirements --> deterministic shift decisions
                |
                v
Probabilistic demand scenarios --> workload scenarios
                |
                v
Two-stage staffing optimization --> out-of-sample decision evaluation
```

The principal takeaway is that **forecast value depends on how forecasts are used and on the operational constraints of the decision system**.

## 2. Paper 1 — Systematic literature review

The first study reviews **156 publications** on forecasting in unscheduled care and organizes the literature around three broad dimensions:

1. **Care settings** — where forecasting is applied in unscheduled care.
2. **Planning levels** — the strategic, tactical, and operational decisions forecasts may support.
3. **Forecasting process** — forecasting purpose, data and targets, methods, evaluation, and reproducibility.

A key research gap is the limited assessment of **downstream decision value**: a forecast can score well statistically without necessarily improving the planning decisions it supports. The review also highlights opportunities for stronger probabilistic evaluation and reproducible research practices.

**Associated publication**

Shi, M., Rostami-Tabar, B., & Gartner, D. (2025). *Looking for the Crystal Ball in Unscheduled Care* [verify full publication title and citation details before finalizing]. *Health Care Management Science*. https://doi.org/10.1007/s10729-025-09711-z

**Code/materials:** Review protocols, screening records, extraction tables, or analytical scripts can be documented here **only if they are included and permitted for redistribution**. A systematic review may have fewer executable code artifacts than the forecasting and optimization studies.

## 3. Paper 2 — Point forecasting and deterministic staffing optimization

The second study evaluates how **point forecasts of emergency department arrivals** perform once they are converted into staffing decisions.

### 3.1 Forecasting setup

- Emergency department arrival data from Wales, covering approximately **five years**.
- **15-minute** time intervals, with **96 intervals per day**.
- Approximately four years used before the final **365-day** out-of-sample evaluation period.
- Multiple statistical, machine learning, and other forecasting approaches are compared. The research has considered methods such as seasonal naïve forecasts, ETS, ARIMA, regression-based methods, GAM, Prophet, and selected machine-learning/deep-learning approaches; consult the specific experiment scripts for the exact model set used in each paper.
- Forecast evaluation measures include **ME, MAE, RMSE, MASE**, and other measures where implemented.

### 3.2 Forecast-to-staffing workflow

```text
15-minute arrival history
        |
        v
Daily point forecasts (96 time steps)
        |
        v
Estimated staffing requirements
        |
        v
Deterministic shift-coverage optimization
        |
        v
Evaluation against realized demand
        |
        v
Understaffing / overstaffing / staffing imbalance
```

A simplified workload mapping uses a **14-interval rolling window (3.5 hours)** and a **patient-to-nurse conversion factor of 0.25**. These are **model assumptions**, not a complete representation of clinical workload or actual hospital staffing policy.

The optimization uses feasible shift patterns and staffing coverage constraints. Importantly, staffing outcomes are evaluated against **realized demand**, rather than assessed only through statistical forecast scores.

### 3.3 Key insight

Forecasts with similar MAE or RMSE can still yield different staffing decisions. The **direction and timing** of errors matter because understaffing and overstaffing have different operational consequences, and discrete shift patterns constrain available decisions. In the empirical analysis, **mean error (ME)** was strongly and consistently associated with **signed staffing imbalance** across much of the model comparison; it should not be interpreted as a universal substitute for other accuracy measures.

## 4. Paper 3 — Probabilistic forecasting and two-stage stochastic optimization

The third study explicitly incorporates **forecast uncertainty** into staffing decisions.

### 4.1 Probabilistic forecasts and scenarios

Rather than supplying only one expected arrival value for each interval, the forecast/scenario pipeline represents **multiple possible demand paths**. These paths are converted into staffing-requirement scenarios. Scenario construction and reduction (including bootstrap-based generation and k-medoids clustering where applied) should be traced to the actual code and experiment configuration.

Probabilistic forecast quality is assessed using **CRPS (Continuous Ranked Probability Score)**. CRPS measures distributional predictive performance; it does **not** directly measure staffing cost.

### 4.2 Two-stage decision model

- **First stage:** choose regular staffing shifts **before** the future demand scenario is known.
- **Second stage:** after demand is realized, allow modelled recourse or flexible capacity, and account for any remaining shortage and excess staffing.

The analysis considers several related optimization quantities:

| Term | Meaning |
| --- | --- |
| **EV** | Expected-value optimization problem: choose a staffing plan using expected demand. |
| **EEV** | Expected cost of applying the EV solution across uncertain demand scenarios, with the appropriate second-stage evaluation. |
| **RP** | Recourse problem: choose the first-stage staffing plan while explicitly considering multiple scenarios. |
| **VSS** | Value of the stochastic solution; in a cost-minimization setting, conventionally **EEV − RP** under a consistent scenario distribution and cost definition. |
| **EVPI** | Expected value of perfect information; in a cost-minimization setting, conventionally **RP − WS**, where WS is the wait-and-see benchmark with scenario information available before the first-stage decision. |

**Important:** EV, EEV, RP, VSS, and EVPI must be computed with consistent objective definitions, comparable distributions, and clearly distinguished optimization versus evaluation scenarios. Refer to the actual scripts and manuscript for exact implementation and units.

### 4.3 Findings and interpretation

- **Similar CRPS does not necessarily imply similar staffing performance.** Distributional accuracy and decision value are related but distinct.
- **Scenario-based planning can reduce severe understaffing and exposure to high-cost outcomes** in the evaluated model settings.
- **Operational flexibility changes the incremental value of stochastic planning.** When staffing can be adjusted easily after demand becomes known, sophisticated advance planning may provide less additional benefit; when adjustment is constrained, explicitly accounting for uncertainty may be more valuable.

This last pattern is discussed in the thesis as a **flexibility paradox**. It is a **conditional, model-based finding**, not a general claim that stochastic optimization always outperforms a simpler plan.

The cost figures used in scenario and sensitivity experiments are **model cost units**, not demonstrated cash savings achieved by a hospital.

## 5. Suggested repository organization

The following is a **proposed** structure, not a claim that these files already exist:


## 6. Reproducibility and running the experiments

The research workflows have used **R** and relevant forecasting/optimization tooling. The exact dependencies and run commands **cannot be verified without inspecting the committed source code**. Accordingly, this README does not claim that a single command reproduces all thesis results.



### Data availability and privacy

The empirical studies use emergency department data. **Do not upload patient-level or institutionally restricted data to a public repository without explicit authorization.** Replace any private file paths, keys, personal identifiers, or credentials with configuration templates. If access to the original dataset is restricted, describe the access process only when you have permission to do so. A synthetic demonstration dataset can be helpful, but must be labelled as synthetic.

## 7. Limitations and responsible interpretation

- Staffing requirements use a **simplified demand-to-workload conversion** and do not capture all dimensions of clinical case mix, acuity, skills, or rostering requirements.
- The optimization results describe **modelled staffing costs and constraints** rather than measured improvements in patient outcomes or verified financial savings.
- Results are **context-dependent**: different costs, staffing flexibility, demand patterns, and shift rules can change which approach performs best.
- The repository should not be treated as an operational staffing system suitable for deployment without local clinical validation, governance, and safety review.

## 8. Publications and citation

If you use code from this repository, please cite the relevant associated paper(s) and, when available, a versioned software release.

**Paper 1 (published):**

Shi, M., Rostami-Tabar, B., & Gartner, D. (2025). [Verify exact article title in the publisher record]. *Health Care Management Science*. https://doi.org/10.1007/s10729-025-09711-z

**Papers 2 and 3:** Bibliographic details and publication status should be updated when publicly confirmed. Do not describe a manuscript as accepted or published unless that status is verified.

## 9. Acknowledgements

This PhD research was supported by a **Cardiff Business School Studentship**. The author gratefully acknowledges the guidance and support of **Bahman Rostami-Tabar** and **Daniel Gartner** at Cardiff University.

## 10. Contact and contributions

**Researcher:** Mingzhe Shi  
**Institution:** Cardiff Business School, Cardiff University

For research questions, please use GitHub Issues if enabled. Contributions and reuse permissions will depend on the licence chosen for this repository. Please do not submit confidential clinical data or proprietary material through public issues or pull requests.
