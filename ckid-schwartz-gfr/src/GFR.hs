module GFR where
type Height = Double -- | Height in meters
type Scr    = Double -- | Serum creatinine in mg/dl
type Cys    = Double -- | Cystatin C in mg/L
type Bun    = Double -- | Blood Urea Nitrogen in mg/dl
data Gender = Male | Female   -- | Male of Female

-- | Calculate the Glomerular Filtration Rate by the CKiD Schwartz method
gfr::Height -> Scr -> Cys -> Bun -> Gender -> Double
gfr ht scr cys bun g = htScrRatio * cysRatio * bunRatio * htRatio * genderFactor
    where
      htScrRatio   = 39.1 * (ht/scr)**0.516
      cysRatio     = (1.8/cys)**0.294
      bunRatio     = (30/bun)**0.169
      htRatio      = (ht/1.4)**0.188
      genderFactor = case g of
                       Male   -> 1.099
                       Female -> 1.0

