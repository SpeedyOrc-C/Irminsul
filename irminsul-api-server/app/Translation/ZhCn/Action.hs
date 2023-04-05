module Translation.ZhCn.Action where

import Control.Arrow (Arrow(first))
import Irminsul


translationActionZhCn = first Action <$> [
    ("Mother", "母亲"),
    ("Father", "父亲"),
    ("Son", "儿子"),
    ("Daughter", "女儿"),
    
    ("YoungerSister", "妹妹"),
    ("ElderSister", "姐姐"),
    ("YoungerBrother", "弟弟"),
    ("ElderBrother", "哥哥"),
    ("FosterYoungerSister", "干妹妹"),
    ("FosterElderSister", "干姐姐"),
    ("FosterYoungerBrother", "干弟弟"),
    ("FosterElderBrother", "干哥哥"),

    ("Friend", "好友"),
    ("Student", "学生"),
    ("Teacher", "老师"),
    ("Guardian", "监护人"),
    ("ActingGuardian", "代理监护人"),
    ("ActingGrandMaster", "代理团长"),
    ("TeamCaptain", "队长"),
    ("LiveIn", "居住"),
    ("Love", "爱"),
    ("Create", "创造"),
    ("Kill", "杀"),

    ("SupportInSilence", "暗中照顾")
    ]
