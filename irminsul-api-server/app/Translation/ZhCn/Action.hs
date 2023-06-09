module Translation.ZhCn.Action where

import Control.Arrow (Arrow(first))
import Irminsul


translationActionZhCn = first Action <$> [
    ("ActingGrandMaster", "代理团长"),
    ("ActingGuardian", "代理监护人"),
    ("Admire", "仰慕"),
    ("AllergicTo", "过敏"),
    ("Attendant", "扈从"),
    ("Create", "创造"),
    ("Daughter", "女儿"),
    ("Dislike", "不喜欢"),
    ("ElderBrother", "哥哥"),
    ("ElderSister", "姐姐"),
    ("Familiar", "眷属"),
    ("Father", "父亲"),
    ("FosterElderBrother", "干哥哥"),
    ("FosterElderSister", "干姐姐"),
    ("FosterYoungerBrother", "干弟弟"),
    ("FosterYoungerSister", "干妹妹"),
    ("Friend", "好友"),
    ("GrandMaster", "团长"),
    ("Guardian", "监护人"),
    ("Hate", "讨厌"),
    ("Kill", "杀"),
    ("Like", "喜欢"),
    ("LiveIn", "居住"),
    ("Love", "爱"),
    ("Mother", "母亲"),
    ("Partner", "搭档"),
    ("Pet", "宠物"),
    ("Rule", "统治"),
    ("Son", "儿子"),
    ("Student", "学生"),
    ("SupportInSilence", "暗中照顾"),
    ("TavernOwner", "酒馆老板"),
    ("Teacher", "老师"),
    ("TeamCaptain", "队长"),
    ("Translator", "译者"),
    ("YoungerBrother", "弟弟"),
    ("YoungerGeneration", "前辈"),
    ("YoungerSister", "妹妹")
    ]
