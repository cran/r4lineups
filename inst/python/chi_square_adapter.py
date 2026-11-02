"""Correct rejection-cell variances for the pinned pyWitness fit instance.

The audited upstream revision reverses observed/expected denominators for
rejection cells. This adapter retains its forward probabilities and penalties,
but makes the configured discrepancy consistent across the response partition.
It changes no installed module or class.
"""

from types import MethodType
from time import perf_counter
import numpy as np
from scipy.optimize import minimize
from scipy.stats import chi2


def correct_chi_square_objective(fit):
    original = fit.calculateChi2

    def corrected(self, params):
        value = original(params)
        predicted = self.calculateFrequenciesForAllCriteria()
        observed = (
            float(self.processedData.data_pivot.loc['targetAbsent', 'rejectId'].sum()),
            float(self.processedData.data_pivot.loc['targetPresent', 'rejectId'].sum()),
        )
        expected = (float(predicted[0]), float(predicted[3]))
        for obs, exp in zip(observed, expected):
            old_denom, new_denom = ((obs, exp) if self.chi2Var == 'expected'
                                    else (exp, obs))
            if old_denom <= 0 or new_denom <= 0:
                return float('inf')
            value += (obs - exp) ** 2 * (1 / new_denom - 1 / old_denom)
        return float(value) if np.isfinite(value) else float('inf')

    fit.calculateChi2 = MethodType(corrected, fit)

    def fitted(self, maxiter=5000, method="Nelder-Mead", resetParameters=False):
        if resetParameters:
            self.resetParameters()
        free = self.freeParameterList()
        initial = [float(parameter.value) for parameter in free]
        self.iteration = 0
        self.chi2_array = []
        options = {"maxiter": maxiter}
        if method == "Nelder-Mead":
            options.update(xatol=1e-8, fatol=1e-8)
        started = perf_counter()
        optimum = minimize(self.calculateChi2, initial, method=method, options=options)
        # The upstream method leaves the parameters at its last evaluation,
        # which need not be the optimizer's returned minimum.
        for parameter, value in zip(free, optimum.x):
            parameter.value = float(value)
        self.numberIterations = int(optimum.nit)
        self.fitStatus = str(optimum.message)
        self.timeDiff = perf_counter() - started
        self.pValue = float(chi2.sf(self.chi2, self.numberDegreesOfFreedom))

    fit.fit = MethodType(fitted, fit)
    return fit
