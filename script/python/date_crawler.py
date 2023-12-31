import sys
from bs4 import BeautifulSoup
import re
import typing
import requests
import urllib.parse
import json
import time

"""
This file is used to crawl the era data from https://sinocal.sinica.edu.tw.
Note that it is designed to be a one-off script, so it is not very robust.

In fact, the website's API is not very robust either, for example it sometimes
forgets to return the first month of an era, and sometimes when the second
emperor does not change the era name, it still treats it as a new era starting
from Year 1. So we have to do some manual processing afterwards.

The output is then fed to data_convert.py to convert the dates into integers.
"""

# List of all dynasties, emperors, and eras
# Note that it is slightly different with the dynasties.json file. This one
# was directly copied from the data source (after processing of course), but
# the other one has some manual changes for consistency.
jsonRaw = '''
[
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "元始"
                    }
                ],
                "name": "平帝"
            },
            {
                "eras": [
                    {
                        "name": "居攝"
                    },
                    {
                        "name": "初始"
                    }
                ],
                "name": "孺子嬰"
            }
        ],
        "name": "西漢"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "始建國"
                    },
                    {
                        "name": "天鳳"
                    },
                    {
                        "name": "地皇"
                    }
                ],
                "name": "王莽"
            }
        ],
        "name": "新"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "更始"
                    }
                ],
                "name": "淮陽王"
            }
        ],
        "name": "更始朝"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "建武"
                    },
                    {
                        "name": "建武中元"
                    }
                ],
                "name": "光武帝"
            },
            {
                "eras": [
                    {
                        "name": "永平"
                    }
                ],
                "name": "明帝"
            },
            {
                "eras": [
                    {
                        "name": "建初"
                    },
                    {
                        "name": "元和"
                    },
                    {
                        "name": "章和"
                    }
                ],
                "name": "章帝"
            },
            {
                "eras": [
                    {
                        "name": "永元"
                    },
                    {
                        "name": "元興"
                    }
                ],
                "name": "和帝"
            },
            {
                "eras": [
                    {
                        "name": "延平"
                    }
                ],
                "name": "殤帝"
            },
            {
                "eras": [
                    {
                        "name": "永初"
                    },
                    {
                        "name": "元初"
                    },
                    {
                        "name": "永寧"
                    },
                    {
                        "name": "建光"
                    },
                    {
                        "name": "延光"
                    }
                ],
                "name": "安帝"
            },
            {
                "eras": [
                    {
                        "name": "延光"
                    }
                ],
                "name": "北鄉侯"
            },
            {
                "eras": [
                    {
                        "name": "永建"
                    },
                    {
                        "name": "陽嘉"
                    },
                    {
                        "name": "永和"
                    },
                    {
                        "name": "漢安"
                    },
                    {
                        "name": "建康"
                    }
                ],
                "name": "順帝"
            },
            {
                "eras": [
                    {
                        "name": "永嘉"
                    }
                ],
                "name": "沖帝"
            },
            {
                "eras": [
                    {
                        "name": "本初"
                    }
                ],
                "name": "質帝"
            },
            {
                "eras": [
                    {
                        "name": "建和"
                    },
                    {
                        "name": "和平"
                    },
                    {
                        "name": "元嘉"
                    },
                    {
                        "name": "永興"
                    },
                    {
                        "name": "永壽"
                    },
                    {
                        "name": "延熹"
                    },
                    {
                        "name": "永康"
                    }
                ],
                "name": "桓帝"
            },
            {
                "eras": [
                    {
                        "name": "建寧"
                    },
                    {
                        "name": "熹平"
                    },
                    {
                        "name": "光和"
                    },
                    {
                        "name": "中平"
                    }
                ],
                "name": "靈帝"
            },
            {
                "eras": [
                    {
                        "name": "光熹"
                    },
                    {
                        "name": "昭寧"
                    }
                ],
                "name": "少帝"
            },
            {
                "eras": [
                    {
                        "name": "永漢"
                    },
                    {
                        "name": "中平"
                    },
                    {
                        "name": "初平"
                    },
                    {
                        "name": "興平"
                    },
                    {
                        "name": "建安"
                    },
                    {
                        "name": "延康"
                    }
                ],
                "name": "獻帝"
            }
        ],
        "name": "東漢"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "黃初"
                    }
                ],
                "name": "文帝"
            },
            {
                "eras": [
                    {
                        "name": "太和"
                    },
                    {
                        "name": "青龍"
                    },
                    {
                        "name": "景初"
                    }
                ],
                "name": "明帝"
            },
            {
                "eras": [
                    {
                        "name": "正始"
                    },
                    {
                        "name": "嘉平"
                    }
                ],
                "name": "少帝"
            },
            {
                "eras": [
                    {
                        "name": "正元"
                    },
                    {
                        "name": "甘露"
                    }
                ],
                "name": "高貴鄉公"
            },
            {
                "eras": [
                    {
                        "name": "景元"
                    },
                    {
                        "name": "咸熙"
                    }
                ],
                "name": "元帝"
            }
        ],
        "name": "魏"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "章武"
                    }
                ],
                "name": "昭烈帝"
            },
            {
                "eras": [
                    {
                        "name": "建興"
                    },
                    {
                        "name": "延熙"
                    },
                    {
                        "name": "景耀"
                    },
                    {
                        "name": "炎興"
                    }
                ],
                "name": "後主"
            }
        ],
        "name": "蜀"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "黃武"
                    },
                    {
                        "name": "黃龍"
                    },
                    {
                        "name": "嘉禾"
                    },
                    {
                        "name": "赤烏"
                    },
                    {
                        "name": "太元"
                    },
                    {
                        "name": "神鳳"
                    }
                ],
                "name": "大帝"
            },
            {
                "eras": [
                    {
                        "name": "建興"
                    },
                    {
                        "name": "五鳳"
                    },
                    {
                        "name": "太平"
                    }
                ],
                "name": "會稽王"
            },
            {
                "eras": [
                    {
                        "name": "永安"
                    }
                ],
                "name": "景帝"
            },
            {
                "eras": [
                    {
                        "name": "元興"
                    },
                    {
                        "name": "甘露"
                    },
                    {
                        "name": "寶鼎"
                    },
                    {
                        "name": "建衡"
                    },
                    {
                        "name": "鳳凰"
                    },
                    {
                        "name": "天冊"
                    },
                    {
                        "name": "天璽"
                    },
                    {
                        "name": "天紀"
                    }
                ],
                "name": "末帝"
            }
        ],
        "name": "吳"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "泰始"
                    },
                    {
                        "name": "咸寧"
                    },
                    {
                        "name": "太康"
                    },
                    {
                        "name": "太熙"
                    }
                ],
                "name": "武帝"
            },
            {
                "eras": [
                    {
                        "name": "永熙"
                    },
                    {
                        "name": "永平"
                    },
                    {
                        "name": "元康"
                    },
                    {
                        "name": "永康"
                    },
                    {
                        "name": "永寧"
                    },
                    {
                        "name": "太安"
                    },
                    {
                        "name": "永安"
                    },
                    {
                        "name": "建武"
                    },
                    {
                        "name": "永安(復稱)"
                    },
                    {
                        "name": "永興"
                    },
                    {
                        "name": "光熙"
                    }
                ],
                "name": "惠帝"
            },
            {
                "eras": [
                    {
                        "name": "永嘉"
                    }
                ],
                "name": "懷帝"
            },
            {
                "eras": [
                    {
                        "name": "建興"
                    }
                ],
                "name": "愍帝"
            }
        ],
        "name": "西晉"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "建武"
                    },
                    {
                        "name": "太興"
                    },
                    {
                        "name": "永昌"
                    }
                ],
                "name": "元帝"
            },
            {
                "eras": [
                    {
                        "name": "太寧"
                    }
                ],
                "name": "明帝"
            },
            {
                "eras": [
                    {
                        "name": "咸和"
                    },
                    {
                        "name": "咸康"
                    }
                ],
                "name": "成帝"
            },
            {
                "eras": [
                    {
                        "name": "建元"
                    }
                ],
                "name": "康帝"
            },
            {
                "eras": [
                    {
                        "name": "永和"
                    },
                    {
                        "name": "升平"
                    }
                ],
                "name": "穆帝"
            },
            {
                "eras": [
                    {
                        "name": "隆和"
                    },
                    {
                        "name": "興寧"
                    }
                ],
                "name": "哀帝"
            },
            {
                "eras": [
                    {
                        "name": "太和"
                    }
                ],
                "name": "海西公"
            },
            {
                "eras": [
                    {
                        "name": "咸安"
                    }
                ],
                "name": "簡文帝"
            },
            {
                "eras": [
                    {
                        "name": "寧康"
                    },
                    {
                        "name": "太元"
                    }
                ],
                "name": "孝武帝"
            },
            {
                "eras": [
                    {
                        "name": "隆安"
                    },
                    {
                        "name": "元興"
                    },
                    {
                        "name": "義熙"
                    }
                ],
                "name": "安帝"
            },
            {
                "eras": [
                    {
                        "name": "元熙"
                    }
                ],
                "name": "恭帝"
            }
        ],
        "name": "東晉"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "永初"
                    }
                ],
                "name": "武帝"
            },
            {
                "eras": [
                    {
                        "name": "景平"
                    }
                ],
                "name": "營陽王"
            },
            {
                "eras": [
                    {
                        "name": "元嘉"
                    }
                ],
                "name": "文帝"
            },
            {
                "eras": [
                    {
                        "name": "孝建"
                    },
                    {
                        "name": "大明"
                    }
                ],
                "name": "孝武帝"
            },
            {
                "eras": [
                    {
                        "name": "永光"
                    },
                    {
                        "name": "景和"
                    }
                ],
                "name": "前廢帝"
            },
            {
                "eras": [
                    {
                        "name": "泰始"
                    },
                    {
                        "name": "泰豫"
                    }
                ],
                "name": "明帝"
            },
            {
                "eras": [
                    {
                        "name": "元徽"
                    }
                ],
                "name": "蒼梧王"
            },
            {
                "eras": [
                    {
                        "name": "昇明"
                    }
                ],
                "name": "順帝"
            }
        ],
        "name": "前宋"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "建元"
                    }
                ],
                "name": "高帝"
            },
            {
                "eras": [
                    {
                        "name": "永明"
                    }
                ],
                "name": "武帝"
            },
            {
                "eras": [
                    {
                        "name": "隆昌"
                    }
                ],
                "name": "鬱林王"
            },
            {
                "eras": [
                    {
                        "name": "延興"
                    }
                ],
                "name": "海陵王"
            },
            {
                "eras": [
                    {
                        "name": "建武"
                    },
                    {
                        "name": "永泰"
                    }
                ],
                "name": "明帝"
            },
            {
                "eras": [
                    {
                        "name": "永元"
                    }
                ],
                "name": "東昏侯"
            },
            {
                "eras": [
                    {
                        "name": "中興"
                    }
                ],
                "name": "和帝"
            }
        ],
        "name": "南齊"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "天監"
                    },
                    {
                        "name": "普通"
                    },
                    {
                        "name": "大通"
                    },
                    {
                        "name": "中大通"
                    },
                    {
                        "name": "大同"
                    },
                    {
                        "name": "中大同"
                    },
                    {
                        "name": "太清"
                    }
                ],
                "name": "武帝"
            },
            {
                "eras": [
                    {
                        "name": "大寶"
                    }
                ],
                "name": "簡文帝"
            },
            {
                "eras": [
                    {
                        "name": "天正"
                    }
                ],
                "name": "豫章王"
            },
            {
                "eras": [
                    {
                        "name": "承聖"
                    }
                ],
                "name": "元帝"
            },
            {
                "eras": [
                    {
                        "name": "天成"
                    }
                ],
                "name": "貞陽侯"
            },
            {
                "eras": [
                    {
                        "name": "紹泰"
                    },
                    {
                        "name": "太平"
                    }
                ],
                "name": "敬帝"
            }
        ],
        "name": "南梁"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "永定"
                    }
                ],
                "name": "武帝"
            },
            {
                "eras": [
                    {
                        "name": "天嘉"
                    },
                    {
                        "name": "天康"
                    }
                ],
                "name": "文帝"
            },
            {
                "eras": [
                    {
                        "name": "光大"
                    }
                ],
                "name": "臨海王"
            },
            {
                "eras": [
                    {
                        "name": "太建"
                    }
                ],
                "name": "宣帝"
            },
            {
                "eras": [
                    {
                        "name": "至德"
                    },
                    {
                        "name": "禎明"
                    }
                ],
                "name": "後主"
            }
        ],
        "name": "陳"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "登國"
                    },
                    {
                        "name": "皇始"
                    },
                    {
                        "name": "天興"
                    },
                    {
                        "name": "天賜"
                    }
                ],
                "name": "道武帝"
            },
            {
                "eras": [
                    {
                        "name": "永興"
                    },
                    {
                        "name": "神瑞"
                    },
                    {
                        "name": "泰常"
                    }
                ],
                "name": "明元帝"
            },
            {
                "eras": [
                    {
                        "name": "始光"
                    },
                    {
                        "name": "神䴥"
                    },
                    {
                        "name": "延和"
                    },
                    {
                        "name": "太延"
                    },
                    {
                        "name": "太平真君"
                    },
                    {
                        "name": "正平"
                    }
                ],
                "name": "太武帝"
            },
            {
                "eras": [
                    {
                        "name": "承平"
                    }
                ],
                "name": "南安王"
            },
            {
                "eras": [
                    {
                        "name": "興安"
                    },
                    {
                        "name": "興光"
                    },
                    {
                        "name": "太安"
                    },
                    {
                        "name": "和平"
                    }
                ],
                "name": "文成帝"
            },
            {
                "eras": [
                    {
                        "name": "天安"
                    },
                    {
                        "name": "皇興"
                    }
                ],
                "name": "獻文帝"
            },
            {
                "eras": [
                    {
                        "name": "延興"
                    },
                    {
                        "name": "承明"
                    },
                    {
                        "name": "太和"
                    }
                ],
                "name": "孝文帝"
            },
            {
                "eras": [
                    {
                        "name": "景明"
                    },
                    {
                        "name": "正始"
                    },
                    {
                        "name": "永平"
                    },
                    {
                        "name": "延昌"
                    }
                ],
                "name": "宣武帝"
            },
            {
                "eras": [
                    {
                        "name": "熙平"
                    },
                    {
                        "name": "神龜"
                    },
                    {
                        "name": "正光"
                    },
                    {
                        "name": "孝昌"
                    },
                    {
                        "name": "武泰"
                    }
                ],
                "name": "孝明帝"
            },
            {
                "eras": [
                    {
                        "name": "建義"
                    },
                    {
                        "name": "永安"
                    }
                ],
                "name": "孝莊帝"
            },
            {
                "eras": [
                    {
                        "name": "建明"
                    }
                ],
                "name": "長廣王"
            },
            {
                "eras": [
                    {
                        "name": "普泰"
                    }
                ],
                "name": "節閔帝"
            },
            {
                "eras": [
                    {
                        "name": "中興"
                    }
                ],
                "name": "安定王"
            },
            {
                "eras": [
                    {
                        "name": "太昌"
                    },
                    {
                        "name": "永興"
                    },
                    {
                        "name": "永熙"
                    }
                ],
                "name": "孝武帝"
            }
        ],
        "name": "北魏"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "大統"
                    }
                ],
                "name": "文帝"
            },
            {
                "eras": [],
                "name": "廢帝"
            },
            {
                "eras": [],
                "name": "恭帝"
            }
        ],
        "name": "西魏"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "天平"
                    },
                    {
                        "name": "元象"
                    },
                    {
                        "name": "興和"
                    },
                    {
                        "name": "武定"
                    }
                ],
                "name": "孝靜帝"
            }
        ],
        "name": "東魏"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "天保"
                    }
                ],
                "name": "文宣帝"
            },
            {
                "eras": [
                    {
                        "name": "乾明"
                    }
                ],
                "name": "廢帝"
            },
            {
                "eras": [
                    {
                        "name": "皇建"
                    }
                ],
                "name": "孝昭帝"
            },
            {
                "eras": [
                    {
                        "name": "大寧"
                    },
                    {
                        "name": "河清"
                    }
                ],
                "name": "武成帝"
            },
            {
                "eras": [
                    {
                        "name": "天統"
                    },
                    {
                        "name": "武平"
                    },
                    {
                        "name": "隆化"
                    }
                ],
                "name": "後主"
            },
            {
                "eras": [
                    {
                        "name": "承光"
                    }
                ],
                "name": "幼主"
            }
        ],
        "name": "北齊"
    },
    {
        "emperors": [
            {
                "eras": [],
                "name": "孝閔帝"
            },
            {
                "eras": [
                    {
                        "name": "*"
                    },
                    {
                        "name": "武成"
                    }
                ],
                "name": "明帝"
            },
            {
                "eras": [
                    {
                        "name": "保定"
                    },
                    {
                        "name": "天和"
                    },
                    {
                        "name": "建德"
                    },
                    {
                        "name": "宣政"
                    }
                ],
                "name": "武帝"
            },
            {
                "eras": [
                    {
                        "name": "大成"
                    }
                ],
                "name": "宣帝"
            },
            {
                "eras": [
                    {
                        "name": "大象"
                    },
                    {
                        "name": "大定"
                    }
                ],
                "name": "靜帝"
            }
        ],
        "name": "北周"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "開皇"
                    },
                    {
                        "name": "仁壽"
                    }
                ],
                "name": "文帝"
            },
            {
                "eras": [
                    {
                        "name": "大業"
                    }
                ],
                "name": "煬帝"
            },
            {
                "eras": [
                    {
                        "name": "義寧"
                    }
                ],
                "name": "恭帝"
            }
        ],
        "name": "隋"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "武德"
                    }
                ],
                "name": "高祖"
            },
            {
                "eras": [
                    {
                        "name": "貞觀"
                    }
                ],
                "name": "太宗"
            },
            {
                "eras": [
                    {
                        "name": "永徽"
                    },
                    {
                        "name": "顯慶"
                    },
                    {
                        "name": "龍朔"
                    },
                    {
                        "name": "麟德"
                    },
                    {
                        "name": "乾封"
                    },
                    {
                        "name": "總章"
                    },
                    {
                        "name": "咸亨"
                    },
                    {
                        "name": "上元"
                    },
                    {
                        "name": "儀鳳"
                    },
                    {
                        "name": "調露"
                    },
                    {
                        "name": "永隆"
                    },
                    {
                        "name": "開耀"
                    },
                    {
                        "name": "永淳"
                    },
                    {
                        "name": "弘道"
                    }
                ],
                "name": "高宗"
            },
            {
                "eras": [
                    {
                        "name": "嗣聖"
                    }
                ],
                "name": "中宗"
            },
            {
                "eras": [
                    {
                        "name": "文明"
                    }
                ],
                "name": "睿宗"
            },
            {
                "eras": [
                    {
                        "name": "光宅"
                    },
                    {
                        "name": "垂拱"
                    },
                    {
                        "name": "永昌"
                    },
                    {
                        "name": "載初"
                    },
                    {
                        "name": "天授"
                    },
                    {
                        "name": "如意"
                    },
                    {
                        "name": "長壽"
                    },
                    {
                        "name": "延載"
                    },
                    {
                        "name": "證聖"
                    },
                    {
                        "name": "天冊萬歲"
                    },
                    {
                        "name": "萬歲登封"
                    },
                    {
                        "name": "萬歲通天"
                    },
                    {
                        "name": "神功"
                    },
                    {
                        "name": "聖曆"
                    },
                    {
                        "name": "久視"
                    },
                    {
                        "name": "大足"
                    },
                    {
                        "name": "長安"
                    },
                    {
                        "name": "神龍"
                    }
                ],
                "name": "武后"
            },
            {
                "eras": [
                    {
                        "name": "神龍"
                    },
                    {
                        "name": "景龍"
                    }
                ],
                "name": "中宗"
            },
            {
                "eras": [
                    {
                        "name": "唐隆"
                    }
                ],
                "name": "殤帝"
            },
            {
                "eras": [
                    {
                        "name": "景雲"
                    },
                    {
                        "name": "太極"
                    },
                    {
                        "name": "延和"
                    }
                ],
                "name": "睿宗"
            },
            {
                "eras": [
                    {
                        "name": "先天"
                    },
                    {
                        "name": "開元"
                    },
                    {
                        "name": "天寶"
                    }
                ],
                "name": "玄宗"
            },
            {
                "eras": [
                    {
                        "name": "至德"
                    },
                    {
                        "name": "乾元"
                    },
                    {
                        "name": "上元"
                    },
                    {
                        "name": "*"
                    },
                    {
                        "name": "寶應"
                    }
                ],
                "name": "肅宗"
            },
            {
                "eras": [
                    {
                        "name": "廣德"
                    },
                    {
                        "name": "永泰"
                    },
                    {
                        "name": "大曆"
                    }
                ],
                "name": "代宗"
            },
            {
                "eras": [
                    {
                        "name": "建中"
                    },
                    {
                        "name": "興元"
                    },
                    {
                        "name": "貞元"
                    }
                ],
                "name": "德宗"
            },
            {
                "eras": [
                    {
                        "name": "永貞"
                    }
                ],
                "name": "順宗"
            },
            {
                "eras": [
                    {
                        "name": "元和"
                    }
                ],
                "name": "憲宗"
            },
            {
                "eras": [
                    {
                        "name": "長慶"
                    }
                ],
                "name": "穆宗"
            },
            {
                "eras": [
                    {
                        "name": "寶曆"
                    }
                ],
                "name": "敬宗"
            },
            {
                "eras": [
                    {
                        "name": "太和"
                    },
                    {
                        "name": "開成"
                    }
                ],
                "name": "文宗"
            },
            {
                "eras": [
                    {
                        "name": "會昌"
                    }
                ],
                "name": "武宗"
            },
            {
                "eras": [
                    {
                        "name": "大中"
                    }
                ],
                "name": "宣宗"
            },
            {
                "eras": [
                    {
                        "name": "咸通"
                    }
                ],
                "name": "懿宗"
            },
            {
                "eras": [
                    {
                        "name": "乾符"
                    },
                    {
                        "name": "廣明"
                    },
                    {
                        "name": "中和"
                    },
                    {
                        "name": "光啟"
                    },
                    {
                        "name": "文德"
                    }
                ],
                "name": "僖宗"
            },
            {
                "eras": [
                    {
                        "name": "龍紀"
                    },
                    {
                        "name": "大順"
                    },
                    {
                        "name": "景福"
                    },
                    {
                        "name": "乾寧"
                    },
                    {
                        "name": "光化"
                    },
                    {
                        "name": "天復"
                    },
                    {
                        "name": "天祐"
                    }
                ],
                "name": "昭宗"
            },
            {
                "eras": [
                    {
                        "name": "天祐"
                    }
                ],
                "name": "哀帝"
            }
        ],
        "name": "唐"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "開平"
                    },
                    {
                        "name": "乾化"
                    }
                ],
                "name": "太祖"
            },
            {
                "eras": [
                    {
                        "name": "鳳曆"
                    }
                ],
                "name": "郢王"
            },
            {
                "eras": [
                    {
                        "name": "乾化"
                    },
                    {
                        "name": "貞明"
                    },
                    {
                        "name": "龍德"
                    }
                ],
                "name": "末帝"
            }
        ],
        "name": "後梁"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "同光"
                    }
                ],
                "name": "莊宗"
            },
            {
                "eras": [
                    {
                        "name": "天成"
                    },
                    {
                        "name": "長興"
                    }
                ],
                "name": "明宗"
            },
            {
                "eras": [
                    {
                        "name": "應順"
                    }
                ],
                "name": "閔帝"
            },
            {
                "eras": [
                    {
                        "name": "清泰"
                    }
                ],
                "name": "廢帝"
            }
        ],
        "name": "後唐"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "天福"
                    }
                ],
                "name": "高祖"
            },
            {
                "eras": [
                    {
                        "name": "天福"
                    },
                    {
                        "name": "開運"
                    }
                ],
                "name": "出帝"
            }
        ],
        "name": "後晉"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "天福"
                    },
                    {
                        "name": "乾祐"
                    }
                ],
                "name": "高祖"
            },
            {
                "eras": [
                    {
                        "name": "乾祐"
                    }
                ],
                "name": "隱帝"
            }
        ],
        "name": "後漢"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "廣順"
                    },
                    {
                        "name": "顯德"
                    }
                ],
                "name": "太祖"
            },
            {
                "eras": [
                    {
                        "name": "顯德"
                    }
                ],
                "name": "世宗"
            },
            {
                "eras": [
                    {
                        "name": "顯德"
                    }
                ],
                "name": "恭帝"
            }
        ],
        "name": "後周"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "建隆"
                    },
                    {
                        "name": "乾德"
                    },
                    {
                        "name": "開寶"
                    }
                ],
                "name": "太祖"
            },
            {
                "eras": [
                    {
                        "name": "太平興國"
                    },
                    {
                        "name": "雍熙"
                    },
                    {
                        "name": "端拱"
                    },
                    {
                        "name": "淳化"
                    },
                    {
                        "name": "至道"
                    }
                ],
                "name": "太宗"
            },
            {
                "eras": [
                    {
                        "name": "咸平"
                    },
                    {
                        "name": "景德"
                    },
                    {
                        "name": "大中祥符"
                    },
                    {
                        "name": "天禧"
                    },
                    {
                        "name": "乾興"
                    }
                ],
                "name": "真宗"
            },
            {
                "eras": [
                    {
                        "name": "天聖"
                    },
                    {
                        "name": "明道"
                    },
                    {
                        "name": "景祐"
                    },
                    {
                        "name": "寶元"
                    },
                    {
                        "name": "康定"
                    },
                    {
                        "name": "慶曆"
                    },
                    {
                        "name": "皇祐"
                    },
                    {
                        "name": "至和"
                    },
                    {
                        "name": "嘉祐"
                    }
                ],
                "name": "仁宗"
            },
            {
                "eras": [
                    {
                        "name": "治平"
                    }
                ],
                "name": "英宗"
            },
            {
                "eras": [
                    {
                        "name": "熙寧"
                    },
                    {
                        "name": "元豐"
                    }
                ],
                "name": "神宗"
            },
            {
                "eras": [
                    {
                        "name": "元祐"
                    },
                    {
                        "name": "紹聖"
                    },
                    {
                        "name": "元符"
                    }
                ],
                "name": "哲宗"
            },
            {
                "eras": [
                    {
                        "name": "建中靖國"
                    },
                    {
                        "name": "崇寧"
                    },
                    {
                        "name": "大觀"
                    },
                    {
                        "name": "政和"
                    },
                    {
                        "name": "重和"
                    },
                    {
                        "name": "宣和"
                    }
                ],
                "name": "徽宗"
            },
            {
                "eras": [
                    {
                        "name": "靖康"
                    }
                ],
                "name": "欽宗"
            },
            {
                "eras": [
                    {
                        "name": "建炎"
                    },
                    {
                        "name": "紹興"
                    }
                ],
                "name": "高宗"
            },
            {
                "eras": [
                    {
                        "name": "隆興"
                    },
                    {
                        "name": "乾道"
                    },
                    {
                        "name": "淳熙"
                    }
                ],
                "name": "孝宗"
            },
            {
                "eras": [
                    {
                        "name": "紹熙"
                    }
                ],
                "name": "光宗"
            },
            {
                "eras": [
                    {
                        "name": "慶元"
                    },
                    {
                        "name": "嘉泰"
                    },
                    {
                        "name": "開禧"
                    },
                    {
                        "name": "嘉定"
                    }
                ],
                "name": "寧宗"
            },
            {
                "eras": [
                    {
                        "name": "寶慶"
                    },
                    {
                        "name": "紹定"
                    },
                    {
                        "name": "端平"
                    },
                    {
                        "name": "嘉熙"
                    },
                    {
                        "name": "淳祐"
                    },
                    {
                        "name": "寶祐"
                    },
                    {
                        "name": "開慶"
                    },
                    {
                        "name": "景定"
                    }
                ],
                "name": "理宗"
            },
            {
                "eras": [
                    {
                        "name": "咸淳"
                    }
                ],
                "name": "度宗"
            },
            {
                "eras": [
                    {
                        "name": "德祐"
                    }
                ],
                "name": "恭宗"
            },
            {
                "eras": [
                    {
                        "name": "景炎"
                    }
                ],
                "name": "端宗"
            },
            {
                "eras": [
                    {
                        "name": "祥興"
                    }
                ],
                "name": "昺帝"
            }
        ],
        "name": "宋"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "*"
                    },
                    {
                        "name": "神冊"
                    },
                    {
                        "name": "天贊"
                    },
                    {
                        "name": "天顯"
                    }
                ],
                "name": "太祖"
            },
            {
                "eras": [
                    {
                        "name": "天顯"
                    },
                    {
                        "name": "會同"
                    },
                    {
                        "name": "大同"
                    }
                ],
                "name": "太宗"
            },
            {
                "eras": [
                    {
                        "name": "天祿"
                    }
                ],
                "name": "世宗"
            },
            {
                "eras": [
                    {
                        "name": "應曆"
                    }
                ],
                "name": "穆宗"
            },
            {
                "eras": [
                    {
                        "name": "保寧"
                    },
                    {
                        "name": "乾亨"
                    }
                ],
                "name": "景宗"
            },
            {
                "eras": [
                    {
                        "name": "統和"
                    },
                    {
                        "name": "開泰"
                    },
                    {
                        "name": "太平"
                    }
                ],
                "name": "聖宗"
            },
            {
                "eras": [
                    {
                        "name": "景福"
                    },
                    {
                        "name": "重熙"
                    }
                ],
                "name": "興宗"
            },
            {
                "eras": [
                    {
                        "name": "清寧"
                    },
                    {
                        "name": "咸雍"
                    },
                    {
                        "name": "大康"
                    },
                    {
                        "name": "大安"
                    },
                    {
                        "name": "壽昌"
                    }
                ],
                "name": "道宗"
            },
            {
                "eras": [
                    {
                        "name": "乾統"
                    },
                    {
                        "name": "天慶"
                    },
                    {
                        "name": "保大"
                    }
                ],
                "name": "天祚帝"
            }
        ],
        "name": "遼"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "收國"
                    },
                    {
                        "name": "天輔"
                    }
                ],
                "name": "太祖"
            },
            {
                "eras": [
                    {
                        "name": "天會"
                    }
                ],
                "name": "太宗"
            },
            {
                "eras": [
                    {
                        "name": "天會"
                    },
                    {
                        "name": "天眷"
                    },
                    {
                        "name": "皇統"
                    }
                ],
                "name": "熙宗"
            },
            {
                "eras": [
                    {
                        "name": "天德"
                    },
                    {
                        "name": "貞元"
                    },
                    {
                        "name": "正隆"
                    }
                ],
                "name": "海陵王"
            },
            {
                "eras": [
                    {
                        "name": "大定"
                    }
                ],
                "name": "世宗"
            },
            {
                "eras": [
                    {
                        "name": "明昌"
                    },
                    {
                        "name": "承安"
                    },
                    {
                        "name": "泰和"
                    }
                ],
                "name": "章宗"
            },
            {
                "eras": [
                    {
                        "name": "大安"
                    },
                    {
                        "name": "至寧"
                    }
                ],
                "name": "衛紹王"
            },
            {
                "eras": [
                    {
                        "name": "貞祐"
                    },
                    {
                        "name": "興定"
                    },
                    {
                        "name": "元光"
                    }
                ],
                "name": "宣宗"
            },
            {
                "eras": [
                    {
                        "name": "正大"
                    }
                ],
                "name": "哀宗"
            }
        ],
        "name": "金"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "中統"
                    },
                    {
                        "name": "至元"
                    }
                ],
                "name": "世祖"
            },
            {
                "eras": [
                    {
                        "name": "元貞"
                    },
                    {
                        "name": "大德"
                    }
                ],
                "name": "成宗"
            },
            {
                "eras": [
                    {
                        "name": "至大"
                    }
                ],
                "name": "武宗"
            },
            {
                "eras": [
                    {
                        "name": "皇慶"
                    },
                    {
                        "name": "延祐"
                    }
                ],
                "name": "仁宗"
            },
            {
                "eras": [
                    {
                        "name": "至治"
                    }
                ],
                "name": "英宗"
            },
            {
                "eras": [
                    {
                        "name": "泰定"
                    },
                    {
                        "name": "致和"
                    }
                ],
                "name": "泰定帝"
            },
            {
                "eras": [
                    {
                        "name": "天順"
                    }
                ],
                "name": "天順帝"
            },
            {
                "eras": [
                    {
                        "name": "天曆"
                    }
                ],
                "name": "文宗"
            },
            {
                "eras": [
                    {
                        "name": "天曆"
                    }
                ],
                "name": "明宗"
            },
            {
                "eras": [
                    {
                        "name": "天曆"
                    },
                    {
                        "name": "至順"
                    }
                ],
                "name": "文宗"
            },
            {
                "eras": [
                    {
                        "name": "至順"
                    }
                ],
                "name": "寧宗"
            },
            {
                "eras": [
                    {
                        "name": "元統"
                    },
                    {
                        "name": "至元"
                    },
                    {
                        "name": "至正"
                    }
                ],
                "name": "順帝"
            },
            {
                "eras": [
                    {
                        "name": "宣光"
                    }
                ],
                "name": "昭宗"
            },
            {
                "eras": [
                    {
                        "name": "天元"
                    }
                ],
                "name": "孛兒只斤脫古思帖木兒"
            }
        ],
        "name": "元"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "洪武"
                    }
                ],
                "name": "太祖"
            },
            {
                "eras": [
                    {
                        "name": "建文"
                    }
                ],
                "name": "惠帝"
            },
            {
                "eras": [
                    {
                        "name": "永樂"
                    }
                ],
                "name": "成祖"
            },
            {
                "eras": [
                    {
                        "name": "洪熙"
                    }
                ],
                "name": "仁宗"
            },
            {
                "eras": [
                    {
                        "name": "宣德"
                    }
                ],
                "name": "宣宗"
            },
            {
                "eras": [
                    {
                        "name": "正統"
                    }
                ],
                "name": "英宗"
            },
            {
                "eras": [
                    {
                        "name": "景泰"
                    }
                ],
                "name": "景帝"
            },
            {
                "eras": [
                    {
                        "name": "天順"
                    }
                ],
                "name": "英宗"
            },
            {
                "eras": [
                    {
                        "name": "成化"
                    }
                ],
                "name": "憲宗"
            },
            {
                "eras": [
                    {
                        "name": "弘治"
                    }
                ],
                "name": "孝宗"
            },
            {
                "eras": [
                    {
                        "name": "正德"
                    }
                ],
                "name": "武宗"
            },
            {
                "eras": [
                    {
                        "name": "嘉靖"
                    }
                ],
                "name": "世宗"
            },
            {
                "eras": [
                    {
                        "name": "隆慶"
                    }
                ],
                "name": "穆宗"
            },
            {
                "eras": [
                    {
                        "name": "萬曆"
                    }
                ],
                "name": "神宗"
            },
            {
                "eras": [
                    {
                        "name": "泰昌"
                    }
                ],
                "name": "光宗"
            },
            {
                "eras": [
                    {
                        "name": "天啟"
                    }
                ],
                "name": "熹宗"
            },
            {
                "eras": [
                    {
                        "name": "崇禎"
                    }
                ],
                "name": "思宗"
            },
            {
                "eras": [
                    {
                        "name": "弘光"
                    }
                ],
                "name": "安宗（福王）"
            },
            {
                "eras": [
                    {
                        "name": "隆武"
                    }
                ],
                "name": "紹宗（唐王）"
            },
            {
                "eras": [
                    {
                        "name": "永曆"
                    }
                ],
                "name": "昭宗（桂王）"
            }
        ],
        "name": "明"
    },
    {
        "emperors": [
            {
                "eras": [
                    {
                        "name": "天命"
                    }
                ],
                "name": "太祖"
            },
            {
                "eras": [
                    {
                        "name": "天聰"
                    },
                    {
                        "name": "崇德"
                    }
                ],
                "name": "太宗"
            },
            {
                "eras": [
                    {
                        "name": "順治"
                    }
                ],
                "name": "世祖"
            },
            {
                "eras": [
                    {
                        "name": "康熙"
                    }
                ],
                "name": "聖祖"
            },
            {
                "eras": [
                    {
                        "name": "雍正"
                    }
                ],
                "name": "世宗"
            },
            {
                "eras": [
                    {
                        "name": "乾隆"
                    }
                ],
                "name": "高宗"
            },
            {
                "eras": [
                    {
                        "name": "嘉慶"
                    }
                ],
                "name": "仁宗"
            },
            {
                "eras": [
                    {
                        "name": "道光"
                    }
                ],
                "name": "宣宗"
            },
            {
                "eras": [
                    {
                        "name": "咸豐"
                    }
                ],
                "name": "文宗"
            },
            {
                "eras": [
                    {
                        "name": "同治"
                    }
                ],
                "name": "穆宗"
            },
            {
                "eras": [
                    {
                        "name": "光緒"
                    }
                ],
                "name": "德宗"
            },
            {
                "eras": [
                    {
                        "name": "宣統"
                    }
                ],
                "name": "遜帝"
            }
        ],
        "name": "清"
    }
]
'''

dynasties = json.loads(s=jsonRaw)

# dynasties = [{'name': '明', 'emperors': [{'name': '安宗（福王）', 'eras': [{'name': ''}]}]}]

yearS='年'
headers: dict[str, typing.Any] = {
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36',
    'Origin': 'https://sinocal.sinica.edu.tw',
    'Referer': 'https://sinocal.sinica.edu.tw/',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
    'Cookie': '_ga=GA1.3.93840436.1684785125; _gid=GA1.3.1340074871.1687899539; PHPSESSID=3npjc7v5ocflumaki7esedmj27',
    'Accept-Encoding': 'gzip, deflate, br',
    'Accept-Language': 'zh-CN,zh;q=0.9,en;q=0.8',
    'Content-Type': 'application/x-www-form-urlencoded',
}

for i in dynasties:
    dynasty: str = i['name']
    dynastyEncoded=urllib.parse.quote(string=dynasty)
    for j in i['emperors']:
        emperor: str = j['name']
        emperorEncoded=urllib.parse.quote(string=emperor)
        for k in j['eras']:
            era: str = k['name']
            eraEncoded=urllib.parse.quote(string=era)
            yearC: int = 1

            raw=requests.post(url='https://sinocal.sinica.edu.tw/luso.php', headers=headers
                                , data=f"lstype=2&swDate=1582%2F10%2F15&dyna={dynastyEncoded}&king={emperorEncoded}&reign={eraEncoded}&all=1&yy=&ycanzi=&mm=&dd=&dcanzi=&token=000843d1d8a0a1cf854a1cf672780aed").text
            html = BeautifulSoup(
                    markup=raw, features='html.parser'
                )

            yearTotal=len([1 for td in html.find_all(name='td') if "西元" in td.text])

            while True:
                time.sleep(1)

                raw=requests.post(url='https://sinocal.sinica.edu.tw/luso.php', headers=headers
                                , data=f"lstype=2&swDate=1582%2F10%2F15&dyna={dynastyEncoded}&king={emperorEncoded}&reign={eraEncoded}&yy={'' if yearC == 1 else yearC}&ycanzi=&mm={'' if yearC == 1 else ''}&dd=&dcanzi=&token=000843d1d8a0a1cf854a1cf672780aed").text
                html = BeautifulSoup(
                        markup=raw, features='html.parser'
                    )

                try:
                    yearW=int(typing.cast(re.Match[str], re.search(pattern=r'西元(\d+)年', string=html.find_all(name='tr')[2].text)).group(1))
                except Exception as e:
                    if yearC == 1:
                        print(f"Please check {dynasty} {emperor} {era}: {e}", file=sys.stderr)
                    if yearC != yearTotal + 1:
                        print(f"Please check {dynasty} {emperor} {era} {yearC}: {e}", file=sys.stderr)
                    break
                monthC=0
                isLeapMonth=False
                monthW=0
                dayC=0
                dayW=0
                kannHcie=None
                isFirst=True

                trs = html.find_all(name='tr')[3:]
                # print(html)

                for entry in [entry for tr in [tr.find_all(name=re.compile(r'(th|td)')) for tr in trs] for entry in tr]:
                    content: str = entry.text.strip()
                    if entry.name == "th":
                        if content.startswith("閏"):
                            isLeapMonth = True
                            monthC = int(content[1:])
                        else:
                            isLeapMonth = False
                            monthC = int(content)
                        dayC = 0
                    else:
                        if content == "-":
                            continue
                        if not content[0].isdigit():
                            kannHcie=content
                        else:
                            content=typing.cast(re.Match[str], re.match(r'^[\x00-\x7F]+', entry.text.strip())).group()
                            if "/" in content:
                                monthW, dayW = map(int, content.split(sep="/"))
                                if monthW == 1 and not isFirst:
                                    yearW += 1
                            else:
                                dayW = int(content)
                            dayC += 1
                            if kannHcie:
                                info = {'date': f'{yearW}-{monthW}-{dayW}', 'kaannHcie': kannHcie}
                                # print(info)
                                kannHcie = None
                            row = {'date': f'{yearW}-{monthW}-{dayW}', 'dateCN': {'emperor': emperor, 'dynasty': dynasty, 'era': era, 'year': yearC, 'yearSuffix': yearS, 'month': monthC, 'day': dayC, 'isLeapMonth': isLeapMonth}}
                            print(json.dumps(row, ensure_ascii=False))
                            isFirst = False
                yearC += 1
