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
    ("Friend", "朋友"),
    ("Student", "学生"),
    ("Teacher", "老师"),
    ("Guardian", "监护人"),
    ("Captain", "首领"),
    ("LiveIn", "居住"),
    ("Love", "爱"),
    ("Create", "创造"),
    ("Kill", "杀")
    ]
