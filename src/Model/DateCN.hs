module Model.DateCN where

data MonthCN = HciaengNgiuat
             | GniNgiuat
             | SaamNgiuat
             | SiNgiuat
             | NgoNgiuat
             | LiukNgiuat
             | ChiitNgiuat
             | PaetNgiuat
             | KiuNgiuat
             | HjipNgiuat
             | TongNgiuat
             | LaapNgiuat
  deriving stock (Eq, Ord, Enum, Bounded)

instance Show MonthCN where
  show :: MonthCN -> String
  show HciaengNgiuat = "正月"
  show GniNgiuat     = "二月"
  show SaamNgiuat    = "三月"
  show SiNgiuat      = "四月"
  show NgoNgiuat     = "五月"
  show LiukNgiuat    = "六月"
  show ChiitNgiuat   = "七月"
  show PaetNgiuat    = "八月"
  show KiuNgiuat     = "九月"
  show HjipNgiuat    = "十月"
  show TongNgiuat    = "冬月"
  show LaapNgiuat    = "臘月"

data DayCN = CrhioQiit
           | CrhioGni
           | CrhioSaam
           | CrhioSi
           | CrhioNgo
           | CrhioLiuk
           | CrhioChiit
           | CrhioPaet
           | CrhioKiu
           | CrhioHjip
           | HjipQiit
           | HjipGni
           | HjipSaam
           | HjipSi
           | HjipNgo
           | HjipLiuk
           | HjipChiit
           | HjipPaet
           | HjipKiu
           | GniHjip
           | GnipQiit
           | GnipGni
           | GnipSaam
           | GnipSi
           | GnipNgo
           | GnipLiuk
           | GnipChiit
           | GnipPaet
           | GnipKiu
           | SaamHjip
  deriving stock (Eq, Ord, Enum, Bounded)

instance Show DayCN where
  show :: DayCN -> String
  show CrhioQiit  = "初一"
  show CrhioGni   = "初二"
  show CrhioSaam  = "初三"
  show CrhioSi    = "初四"
  show CrhioNgo   = "初五"
  show CrhioLiuk  = "初六"
  show CrhioChiit = "初七"
  show CrhioPaet  = "初八"
  show CrhioKiu   = "初九"
  show CrhioHjip  = "初十"
  show HjipQiit   = "十一"
  show HjipGni    = "十二"
  show HjipSaam   = "十三"
  show HjipSi     = "十四"
  show HjipNgo    = "十五"
  show HjipLiuk   = "十六"
  show HjipChiit  = "十七"
  show HjipPaet   = "十八"
  show HjipKiu    = "十九"
  show GniHjip    = "二十"
  show GnipQiit   = "廿一"
  show GnipGni    = "廿二"
  show GnipSaam   = "廿三"
  show GnipSi     = "廿四"
  show GnipNgo    = "廿五"
  show GnipLiuk   = "廿六"
  show GnipChiit  = "廿七"
  show GnipPaet   = "廿八"
  show GnipKiu    = "廿九"
  show SaamHjip   = "三十"
