"""
arsw_python — Python translation of ARSW2015-toolkit MATLAB code for Tasks 1b & 1c.

Entry points:
    from arsw_python.estimate_epsilon import run_optimepsilon_TD86
    from arsw_python.recover_fundamentals import run_calcal_TD
"""
from .estimate_epsilon import run_optimepsilon_TD86
from .recover_fundamentals import run_calcal_TD

__all__ = ["run_optimepsilon_TD86", "run_calcal_TD"]
