module Model.DateCN where

import           Model.Literal

data MonthCN = HciaengNgiwaht
             | GniNgiwaht
             | SaamNgiwaht
             | SiNgiwaht
             | NgoNgiwaht
             | LiwkNgiwaht
             | ChiitNgiwaht
             | PaetNgiwaht
             | KiwNgiwaht
             | HjipNgiwaht
             | TongNgiwaht
             | LaapNgiwaht
  deriving stock (Eq, Ord, Enum, Bounded)

instance Show MonthCN where
  show :: MonthCN -> String
  show HciaengNgiwaht = "正月"
  show GniNgiwaht     = "二月"
  show SaamNgiwaht    = "三月"
  show SiNgiwaht      = "四月"
  show NgoNgiwaht     = "五月"
  show LiwkNgiwaht    = "六月"
  show ChiitNgiwaht   = "七月"
  show PaetNgiwaht    = "八月"
  show KiwNgiwaht     = "九月"
  show HjipNgiwaht    = "十月"
  show TongNgiwaht    = "冬月"
  show LaapNgiwaht    = "臘月"

data DayCN = CrhioQiit
           | CrhioGni
           | CrhioSaam
           | CrhioSi
           | CrhioNgo
           | CrhioLiwk
           | CrhioChiit
           | CrhioPaet
           | CrhioKiw
           | CrhioHjip
           | HjipQiit
           | HjipGni
           | HjipSaam
           | HjipSi
           | HjipNgo
           | HjipLiwk
           | HjipChiit
           | HjipPaet
           | HjipKiw
           | GniHjip
           | GnipQiit
           | GnipGni
           | GnipSaam
           | GnipSi
           | GnipNgo
           | GnipLiwk
           | GnipChiit
           | GnipPaet
           | GnipKiw
           | SaamHjip
  deriving stock (Eq, Ord, Enum, Bounded)

instance Show DayCN where
  show :: DayCN -> String
  show CrhioQiit  = "初一"
  show CrhioGni   = "初二"
  show CrhioSaam  = "初三"
  show CrhioSi    = "初四"
  show CrhioNgo   = "初五"
  show CrhioLiwk  = "初六"
  show CrhioChiit = "初七"
  show CrhioPaet  = "初八"
  show CrhioKiw   = "初九"
  show CrhioHjip  = "初十"
  show HjipQiit   = "十一"
  show HjipGni    = "十二"
  show HjipSaam   = "十三"
  show HjipSi     = "十四"
  show HjipNgo    = "十五"
  show HjipLiwk   = "十六"
  show HjipChiit  = "十七"
  show HjipPaet   = "十八"
  show HjipKiw    = "十九"
  show GniHjip    = "二十"
  show GnipQiit   = "廿一"
  show GnipGni    = "廿二"
  show GnipSaam   = "廿三"
  show GnipSi     = "廿四"
  show GnipNgo    = "廿五"
  show GnipLiwk   = "廿六"
  show GnipChiit  = "廿七"
  show GnipPaet   = "廿八"
  show GnipKiw    = "廿九"
  show SaamHjip   = "三十"

data KaanHcie = KapCih
              | QyitTrhiw
              | PiangYiin
              | TengMaw
              | MuwHjiin
              | KihZih
              | KahngNgo
              | SiinMiwui
              | GniimHsiin
              | KwiYiw
              | KapSiwet
              | QyitGhui
              | PiangCih
              | TengTrhiw
              | MuwYiin
              | KihMaw
              | KahngHjiin
              | SiinZih
              | GniimNgo
              | KwiMiwui
              | KapHsiin
              | QyitYiw
              | PiangSiwet
              | TengGhui
              | MuwCih
              | KihTrhiw
              | KahngYiin
              | SiinMaw
              | GniimHjiin
              | KwiZih
              | KapNgo
              | QyitMiwui
              | PiangHsiin
              | TengYiw
              | MuwSiwet
              | KihGhui
              | KahngCih
              | SiinTrhiw
              | GniimYiin
              | KwiMaw
              | KapHjiin
              | QyitZih
              | PiangNgo
              | TengMiwui
              | MuwHsiin
              | KihYiw
              | KahngSiwet
              | SiinGhui
              | GniimCih
              | KwiTrhiw
              | KapYiin
              | QyitMaw
              | PiangHjiin
              | TengZih
              | MuwNgo
              | KihMiwui
              | KahngHsiin
              | SiinYiw
              | GniimSiwet
              | KwiGhui
  deriving stock (Eq, Ord, Enum, Bounded)

instance Show KaanHcie where
  show :: KaanHcie -> String
  show KapCih     = "甲子"
  show QyitTrhiw  = "乙丑"
  show PiangYiin  = "丙寅"
  show TengMaw    = "丁卯"
  show MuwHjiin   = "戊辰"
  show KihZih     = "己巳"
  show KahngNgo   = "庚午"
  show SiinMiwui  = "辛未"
  show GniimHsiin = "壬申"
  show KwiYiw     = "癸酉"
  show KapSiwet   = "甲戌"
  show QyitGhui   = "乙亥"
  show PiangCih   = "丙子"
  show TengTrhiw  = "丁丑"
  show MuwYiin    = "戊寅"
  show KihMaw     = "己卯"
  show KahngHjiin = "庚辰"
  show SiinZih    = "辛巳"
  show GniimNgo   = "壬午"
  show KwiMiwui   = "癸未"
  show KapHsiin   = "甲申"
  show QyitYiw    = "乙酉"
  show PiangSiwet = "丙戌"
  show TengGhui   = "丁亥"
  show MuwCih     = "戊子"
  show KihTrhiw   = "己丑"
  show KahngYiin  = "庚寅"
  show SiinMaw    = "辛卯"
  show GniimHjiin = "壬辰"
  show KwiZih     = "癸巳"
  show KapNgo     = "甲午"
  show QyitMiwui  = "乙未"
  show PiangHsiin = "丙申"
  show TengYiw    = "丁酉"
  show MuwSiwet   = "戊戌"
  show KihGhui    = "己亥"
  show KahngCih   = "庚子"
  show SiinTrhiw  = "辛丑"
  show GniimYiin  = "壬寅"
  show KwiMaw     = "癸卯"
  show KapHjiin   = "甲辰"
  show QyitZih    = "乙巳"
  show PiangNgo   = "丙午"
  show TengMiwui  = "丁未"
  show MuwHsiin   = "戊申"
  show KihYiw     = "己酉"
  show KahngSiwet = "庚戌"
  show SiinGhui   = "辛亥"
  show GniimCih   = "壬子"
  show KwiTrhiw   = "癸丑"
  show KapYiin    = "甲寅"
  show QyitMaw    = "乙卯"
  show PiangHjiin = "丙辰"
  show TengZih    = "丁巳"
  show MuwNgo     = "戊午"
  show KihMiwui   = "己未"
  show KahngHsiin = "庚申"
  show SiinYiw    = "辛酉"
  show GniimSiwet = "壬戌"
  show KwiGhui    = "癸亥"

data DateCN = DateCN { dynasty     :: Literal
                     , emperor     :: Literal
                     , era         :: Literal
                     , year        :: Int
                     , yearSuffix  :: Literal
                     , month       :: MonthCN
                     , isLeapMonth :: Bool
                     , day         :: DayCN }
  deriving stock (Eq, Ord)
