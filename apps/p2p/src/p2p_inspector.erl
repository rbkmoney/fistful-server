-module(p2p_inspector).

-type risk_score() :: low | high | fatal.

-export_type([risk_score/0]).
