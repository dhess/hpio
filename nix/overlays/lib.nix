self: super:

{
  lib = (super.lib or {}) // {
    maintainers = super.lib.maintainers // {
      dhess = "Drew Hess <src@drewhess.com>";
    };
  };
}
