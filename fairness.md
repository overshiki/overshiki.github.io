https://arxiv.org/pdf/1610.02413.pdf


for binary classification task $\{ X, Y \in \{ 0, 1 \}  \}$, given a binary decision $\widehat{Y} \in \{ 0, 1 \}$, and a binary protected attribute $A \in \{ 0, 1 \}$

### demographic parity:


$Pr\{\widehat{Y}=1|A=0\} = Pr\{\widehat{Y}=1|A=1\}$

Demographic parity requires that a decision $\widehat{Y}$ be always independent of the protected attributes $A$.

However, as mentioned by Dwork et al, it has two shortcomes:

1. the notion permits that we accept qualified applicants in the demographic $A=0$, but unqualified applicants in the demographic $A=1$, as long as the percentage of acceptance match.
2. if the target variable is accually dependent on protected attributes, demographic parity will not allow the idea predictor $\widehat{A}=A$, thus cripples the utils we hope to achieve.

### equalized odds:

$Pr\{\widehat{Y}=1|A=0, Y=y\} = Pr\{\widehat{Y}=1|A=1, Y=y\}, y \in \{ 0, 1 \}$

Equalized odds requires that a decision $\widehat{Y}$ is independent of the protected attibutes $A$ given true labels $Y$.

The defination aligns nicely with the central goal of building highly accurate classifiers, since $\widehat{Y} = Y$ is always a acceptable solution. However, equalized odds enforces that the accuracy is equaly high in all demographics, punishing models that perform well only on the majority. 