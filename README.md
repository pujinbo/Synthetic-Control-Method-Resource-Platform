# 合成控制法资源平台（Synthetic-Control-Method-Resource-Platform）

## 简介
本仓库聚焦**合成控制法（Synthetic Control Method, SCM）** 及其拓展方法的代码实现与应用指南，旨在为因果推断领域的研究者、学生及实务工作者提供一站式技术资源。  

仓库核心内容涵盖三部分：  
1. **基础实现**：包含经典SCM的核心代码（如Abadie经典框架），支持面板数据的处理、权重估计与反事实构建，附详细注释与示例数据集；  
2. **前沿拓展**：更新近年主流改进方法，重点覆盖溢出效应缓解（如迭代合成法、预设结构法）、多处理组SCM、时间异质性SCM等热点技术；  
3. **应用工具**：提供学术论文复现案例（如政策评估、区域经济研究）。  

代码以Python 和 R 为主，兼顾易用性与可扩展性，可直接适配不同研究场景的数据格式。欢迎星标（Star）关注更新，也期待通过Issues或Pull Request交流改进建议，共同推动SCM方法的实践落地。


## Introduction
This repository focuses on the code implementation and application guidelines of the **Synthetic Control Method (SCM)** and its extensions, aiming to provide a one-stop technical resource for researchers, students, and practitioners in the field of causal inference.  

The core content of the repository includes three parts:  
1. **Basic Implementation**: Core code for classical SCM (e.g., Abadie’s framework), supporting panel data processing, weight estimation, and counterfactual construction, with detailed comments and sample datasets;  
2. **Cutting-Edge Extensions**: Updated with mainstream improved methods in recent years, focusing on spillover effect mitigation (e.g., Iterative SCM, Preset Structure Method), multi-treatment SCM, and time-heterogeneous SCM;  
3. **Application Tools**: Functions for result visualization (e.g., placebo test plots, dynamic effect plots), code for robustness checks (e.g., permutation tests, weight sensitivity analysis), and reproducible cases of academic papers (e.g., policy evaluation, regional economic research).  

Codes are mainly written in Python and R , balancing usability and scalability, and can be directly adapted to data formats in different research scenarios. Feel free to star the repository for updates, and we welcome suggestions via Issues or Pull Requests to jointly promote the practical application of SCM methods.

---

## 仓库索引

本项目目前分为 **方法论仓库** 和 **应用仓库** 两个部分，分别对应合成控制法的**方法发展**与**实际应用**。

**方法论仓库**主要整理 SCM 的各类方法拓展，包括估计方法、统计推断以及近年来出现的一些新思路。每类方法都配有简要说明，并尽量提供可复现的官方或社区代码，便于对照学习。

**应用仓库**则系统梳理了近五年经济学与金融学期刊中 SCM 的应用文献：

* Sheet 1 侧重顶级期刊，用于把握前沿研究中的方法使用方式；
* Sheet 2 按研究领域整理重要期刊应用，方便研究者参考更贴近自身方向的实证范例。

整体目标很简单：**让合成控制法的文献更好找，也更好用**。
欢迎一起维护和讨论。

---

## Repository Index

This project is organized into **two separate repositories**, focusing on **methodology** and **applications** of the Synthetic Control Method (SCM), respectively.

### Methodological Repository

This repository collects major **methodological extensions of SCM**, including recent advances in estimation strategies, statistical inference, and machine-learning–oriented implementations.
For each method, we provide a brief summary of its core idea, typical use cases, and (when available) **reproducible official or community code**. Some commonly used commands are also documented in our related papers.

### Applied Repository

This repository documents **recent applications of SCM in economics and finance**, with a focus on studies published in the past five years.

* **Sheet 1** summarizes applications in **top journals**, helping readers understand how frontier methods are used and how research ideas evolve.
* **Sheet 2** covers applications in major field journals, organized by research area, making it easier to find practical examples close to one’s own topic.

Overall, this project aims to make the SCM literature **easier to navigate and easier to use**, serving both as a structured index and a practical toolbox.
Contributions, suggestions, and corrections are always welcome.

---

## 附：仓库代码对应文献
| folder_name                                                          | Title                                                                                                                                        | DOI                                                                                | 期刊                                              |
| -------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------- | ----------------------------------------------- |
| bpCausal-Pang et al.(2022)                                           | A Bayesian Alternative to Synthetic Control for Comparative Case Studies                                                                     | 10.1017/pan.2021.22                                                                | Political Analysis                              |
| Confidence Sets-Firpo & Possebom(2018)                               | Synthetic Control Method: Inference, Sensitivity Analysis and Confidence Sets                                                                | 10.1515/jci-2016-0026                                                              | Journal of Causal Inference                     |
| Distributional conformal prediction-Chernozhukov et al.(2021)        | Distributional conformal prediction                                                                                                          | 10.1073/pnas.2107794118                                                            | PNAS                                            |
| Imperfect fit-Ferman & Pinto(2021)                                   | Synthetic controls with imperfect pretreatment fit                                                                                           | 10.3982/QE1596                                                                     | Quantitative Economics                          |
| LeaveTwoOutSCI-Lei & Sudijono(2024)                                  | Inference for Synthetic Controls via Refined Placebo Tests                                                                                   | 10.48550/arXiv.2401.07152                                                          | working paper                                   |
| Low-rank approximations-Fernandez et al.(2021)                       | Low-rank approximations of nonseparable panel models                                                                                         | 10.1093/ectj/utab007                                                               | Econometrics Journal                            |
| Proximal Causal Inference-Shi et al.(2023)                           | Theory for Identification and Inference with Synthetic Controls: A Proximal Causal Inference Framework                                       | 10.48550/arXiv.2108.13935                                                          | working paper                                   |
| Robust Conformal Inference Method-Chernozhukov et al.(2021)          | An Exact and Robust Conformal Inference Method for Counterfactual and Synthetic Controls                                                     | 10.1080/01621459.2021.1920957                                                      | Journal of the American Statistical Association |
| SC in interrupted time series-Esposti et al.(2020)                   | Can synthetic controls improve causal inference in interrupted time series evaluations of public health interventions?                       | 10.1093/ije/dyaa152                                                                | International Journal of Epidemiology           |
| scinference-Chernozhukov et al.(2025)                                | A T-Test for Synthetic Controls                                                                                                              | 10.48550/arXiv.1812.10820                                                          | working paper                                   |
| SCM-Debug-Kuosmanen et al.(2021)                                     | Design Flaw of the Synthetic Control Method                                                                                                  | [https://mpra.ub.uni-muenchen.de/106390/](https://mpra.ub.uni-muenchen.de/106390/) | working paper                                   |
| scmSpillover-Cao & Dowd(2019)                                        | Estimation and Inference for Synthetic Control Methods with Spillover Effects                                                                | 10.48550/arXiv.1902.07343                                                          | working paper                                   |
| SCPI-Cattaneo et al.(2022)                                           | Prediction Intervals for Synthetic Control Methods                                                                                           | 10.1080/01621459.2021.1979561                                                      | Journal of the American Statistical Association |
| SCPI-Cattaneo et al.(2025)                                           | Uncertainty Quantification in Synthetic Controls with Staggered Treatment Adoption                                                           | 10.1162/rest_a_01588                                                               | Review of Economics and Statistics              |
| Spillover SCM-Melnychuk(2024)                                        | Synthetic Controls with Spillover Effects: A Comparative Study                                                                               | 10.48550/arXiv.2405.01645                                                          | working paper                                   |
| synthsd-连玉君和李鑫（2022）                                                 | 合成控制法中的安慰剂检验改进研究——基于准标准化转换的统计推断                                                                                                              | /                                                                                  | 统计研究                                            |
| ASCM-Ben-Michael et al. (2018)                                       | The Augmented Synthetic Control Method                                                                                                       | 10.3386/w28885                                                                     | Journal of the American Statistical Association |
| Bayesian Nonparametric Common Atoms Regression-Chandra et al. (2023) | Bayesian Nonparametric Common Atoms Regression for Generating Synthetic Controls in Clinical Trials                                          | 10.1080/01621459.2023.2231581                                                      | Journal of the American Statistical Association |
| BSCM-Kim (2020)                                                      | Bayesian Synthetic Control Methods                                                                                                           | 10.1177/0022243720936230                                                           | Journal of Marketing Research                   |
| DR Proximal SC-Qiu et al.(2024)                                      | Doubly robust proximal synthetic controls                                                                                                    | 10.1093/biomtc/ujae055                                                             | Biometrics                                      |
| DSC-Gunsilius(2023)                                                  | DISTRIBUTIONAL SYNTHETIC CONTROLS                                                                                                            | 10.3982/ECTA18260                                                                  | Journal of Econometrics                         |
| Gsynth-Xu(2017)                                                      | Generalized Synthetic Control Method: Causal Inference with Interactive Fixed Effects Models                                                 | 10.1017/pan.2016.2                                                                 | Political Analysis                              |
| MASC-Kellogg et al.(2021)                                            | Combining Matching and Synthetic Control to Tradeoff Biases From Extrapolation and Interpolation                                             | 10.1080/01621459.2021.1979562                                                      | Journal of the American Statistical Association |
| Microsynth-Robbins et al.(2017)                                      | A Framework for Synthetic Control Methods with High-Dimensional, Micro-Level Data: Evaluating a Neighborhood-Specific Crime Intervention     | 10.1080/01621459.2016.1213634                                                      | Journal of the American Statistical Association |
| mlsynth-Bayani(2021)                                                 | Robust PCA Synthetic Control                                                                                                                 | 10.48550/arXiv.2108.12542                                                          | working paper                                   |
| Pensynth-Abadie and L'Hour(2021)                                     | A Penalized Synthetic Control Estimator for Disaggregated Data                                                                               | 10.1080/01621459.2021.1971535                                                      | Journal of the American Statistical Association |
| RCM-Hsiao et al.(2012)                                               | A Panel Data Approach for Program Evaluation – Measuring the Benefits of Political and Economic Integration of Hong Kong with Mainland China | 10.1002/jae.1230                                                                   | Journal of Applied Econometrics                 |
| Robust PCA SCM -Bayani(2021)                                         | Robust PCA Synthetic Control                                                                                                                 | 10.48550/arXiv.2108.12542                                                          | working paper                                   |
| Robust SCM-Amjad et al.(2017)                                        | Robust Synthetic Control                                                                                                                     | 10.48550/arXiv.1711.06940                                                          | Journal of Machine Learning Research            |
| SC with Time Varying Coefficients-Klinenberg(2023)                   | Synthetic Control with Time Varying Coefficients: A State Space Approach with Bayesian Shrinkage                                             | 10.1080/07350015.2022.2102025                                                      | Journal of Business & Economic Statistics       |
| SCM-Abadie et al.(2015)                                              | Comparative Politics and the Synthetic Control Method                                                                                        | 10.1111/ajps.12116                                                                 | American Journal of Political Science           |
| SCUL-Hollingsworth & Wing(2020)                                      | Tactics for design and inference in synthetic control studies: An applied example using high-dimensional data                                | 10.2139/ssrn.3592088                                                               | working paper                                   |
| SDID-Arkhangelsky et al.(2021)                                       | Synthetic Difference in Differences                                                                                                          | 10.1257/aer.20190159                                                               | American Economic Review                        |
| Synthetic Combinations-Agarwal et al.(2023)                          | Synthetic Combinations: A Causal Framework for Combinatorial Interventions                                                                   | 10.48550/arXiv.2303.14226                                                          | working paper                                   |
| Synthetic Learn-Viviano & Bradic(2022)                               | Synthetic Learner: Model-free inference on treatments over time                                                                              | 10.1016/j.jeconom.2022.07.006                                                      | Journal of Econometrics                         |
| TBSCM-Mühlbach & Nielsen (2021)                                      | Tree-based synthetic control methods: Consequences of relocating the US embassy                                                              | 10.48550/arXiv.1909.03968                                                          | working paper                                   |
| Time-Series Cross-Sectional Data-Imai et al.(2021)                   | Matching Methods for Causal Inference with Time-Series Cross-Sectional Data                                                                  | 10.1111/ajps.12685                                                                 | American Journal of Political Science           |
| Properties of SC-Ferman(2021)                                        | On the Properties of the Synthetic Control Estimator with Many Periods and Many Controls                                                     | 10.1080/01621459.2021.1965613                                                      | Journal of the American Statistical Association |
| Review-Liu et al.(2024)                                              | A Practical Guide to Counterfactual Estimators for Causal Inference with Time-Series Cross-Sectional Data                                    | 10.1111/ajps.12723                                                                 | American Journal of Political Science           |
| Review-Yılmaz et al.(2024)                                           | Causal inference under selection on observables in operations management research: Matching methods and synthetic controls                   | 10.1002/joom.1318                                                                  | Journal of Operations Management                |

