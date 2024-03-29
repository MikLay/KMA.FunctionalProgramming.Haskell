Робота №10
----------

*Синтаксичний аналіз XML*

Алфавіт -- скінченна множина символів: букви, цифри, спеціальні символи.
Як правило підмножина елементів типу Char.

Мова L в алфавіті V -- це довільна підмножина слів в алфавіті V, як
правило, нескінченна. Скінченна множина правил, яка описує як побудувати
всі слова мови -- синтаксис мови.

Задача синтаксичного аналізу -- для заданих мови L і слова w визначити
чи містить мова L слово w. Програми, що розв'язують цю задачу --
аналізатори.

Перш ніж розв'язувати задачу аналізу, часто потрібно спочатку описати
саму мову -- задати синтаксис мови.

Існує багато формальних методів, котрі задають синтаксис мов:
контекстно-вільні граматики, форми Наура-Бекуса, синтаксичні діаграми і
т.і. Часто сам формальний метод задання мови визначає структуру і склад
аналізатора.

Для опису мов буде використовуватися форми Наура-Бекуса.

В описі мови використовуються термінали -- це символи алфавіту -- і
нетермінали, котрі позначають певні синтаксичні конструкції мови.

Термінали позначають самі себе і записуються в лапках -- *'a', 'n', '=',
'\\n'*. *'eos'* -- особливий термінал, котрий позначає кінець слова (end
Of String).

Нетермінали -- це слова в кутових дужках - *\<sign\>*, *\<integer\>*,
*\<digit\>*.

Опис мови -- це скінченна множина правил виду:

*нетермінал* ::= *правило*

Правило - слово з терміналів, нетерміналів і метасимволів, котре описує
як побудувати всі слова синтаксичної конструкції *нетермінал.*

Наступні правила описують імена об»єктів *\<name\>* . Ім»я -- непорожня
послідовність символів, що починається з букви і може включати букви,
десяткові цифри та символи '.' (крапка) i '-' (тире) . Ім'я не може
містити проміжки.

*\<sD\> ::= ... довільна десяткова цифра (її можна розпізнати предикатом
isDigit)*

*\<sL\> :: = ... довільна буква (велика або маленька) латинського
алфавіту (isLetter)*

*\<sN\> ::= \<sL\> \| \<sD\> \| '.' \| '-'*

*\<manyN\> ::= \<sN\> \<manyN\> \| ε*

*\<name\> ::= \<sL\> \<manyN\>*

В цих правилах вживаються метасимволи:

-   *a1 \| a2* - вибір одного з варіантів a1 або a2

-   *ε* - позначає порожнє слово ""

-   *\[a1\]* -- конструкцію a1 в середині \[\] потрібно використати 0
    або 1 раз

-   *{a1}* -- конструкцію a1 в середині {} потрібно використати 0 або
    багато разів

Використання метасимволів {} надає можливостей одним правилом описати
нескінченну

множину слів.

Ідея синтаксичного аналізу зверху вниз досить проста.

Створюється набір рекурсивних аналізаторів (функцій, процедур) кожний з
яких розпізнає одну синтаксичну конструкцію (*нетермінал*). Структура
аналізатора відображає правило для цього нетермінала:

-   *термінал --* якщо наступний необроблений символ рядка співпадає з
    терміналом, то аналіз *продовжується* на наступному символі, якщо НЕ
    співпадає, то *помилка*.

-   *нетермінал --* викликається аналізатор, що розпізнає цю синтаксичну
    конструкцію.

Проблема виникає про розпізнаванні правил виду *a1 \| a2* - необхідно
почати розпізнавати або конструкцію *a1* або конструкцію *a2. Найчастіше
рішення приймається н*а основі якогось критерію, наприклад в залежності
від наступного символу котрий потрібно розпізнати, вибирається один з
варіантів і виконується лише він. У випадку невдалої БНФ, що описує
конструкцію, можлива ситуація помилку, хоча насправді інший варіант
приводить до успіху.

The E**X**tensible **M**arkup **L**anguage (розширена мова розмітки або
*XML*), один із стандартів для зберігання та передачі даних. Кожний
XML-документ -- це текстовий рядок певної структури, що включає різні
компоненти.

XML-документи включає різні послідовності символів, кожна з яких
обробляються як одне ціле, і в залежності від контексту або ігнорується
або входить в окрему компоненту документу: *\<name\>, \<spaces\>,
\<fullVal\>, \<text\>*.

*\<sW\> :: ... символи проміжку (її можна розпізнати предикатом
isSpace)*

*\<spaces\> :: = \<sW\> \<spaces\> \| ε*

*\<cV\> ::= ... довільний символ крім символу '"'.*

*\<value\> ::= \<cV\> \<value\> \> \| ε*

*\<fullVal\>::= '"' \<value\> '"'*

*\<sT\> ::= ... довільний символ, крім символів '\<' і '\>'.*

*\<text\> ::= \<sT\> \<manyT\>*

*\<manyT\> ::= \<sT\> \<manyT\> \> \| ε*

Головна одиниця XML документу -- елемент *\<element\>*, котрий має ім'я
*\<name\>* і, може мати, атрибути *\<manyAtt\>* і компоненти
*\<manyXML\>*: текстові поля *\<text\>* та інших «дочірні» XML елементи
*\< element\>*. Структурно елемент включає початковий тег*\<begTag\>*,
що містить ім\`я та атрибути*\<manyAtt\>*, компоненти і заключний тег
*\<endTag\>*, містить лише ім\`я. Кожний атрибут *\<attrib\>* має ім\`я
*\<name\>* і значення *\<fullVal\>*.

Проміжки між елементами початкового тегу -- ім\`я елементу *\<name\>* та
компонентами атрибутів (*\<name\>, '=', \<fullVal\>*) -- не мають ніякої
ваги, тобто НЕ являються частиною даних.Проміжки в *тестовому полі*
*\<text\>* являються частиною даних.

XML документ *\<fullXML\>* -- це елемент, перед яким і після якого
можуть знаходитися проміжки, котрі ігноруються.

Наступні БНФ повністю описують синтаксис XML документ

*\<element\> ::= \<begTag\> \<manyXML\> \<endTag\>*

*\<begTag\> ::= '\<' \<name\> \<space\> \<manyAtt\> '\>'*

*\<manyAtt\> ::= \<attrib\> \<manyAtt\> \| ε*

*\<attrib\> ::= \<name\> \<spaces\> '=' \<spaces\> \<fullVal\>
\<spaces\>*

*\<endTag\> ::= '\<''/\' \<name\> '\>'*

*\<manyXML\>::= \<xml\> \<manyXML\> \| ε*

*\<xml\> ::= \<element\> \| \<text\>*

*\<fullXML\> ::= \<spaces\> \<element\> \<spaces\> 'eos'*

Синтаксичний аналіз як правило не лише розпізнає синтаксично вірні
конструкції, а і будує їх представлення у вигляді Абстрактного
Синтаксичного Дерева AST - *Abstract Syntax Trees. Наступні типи даних*
в Haskell задають Абстрактне Синтаксичне Дерево, що представляє
XML-документ.

***type*** Name = String

***type*** Attributes = \[(String, String)\]

***data*** XML = Text String \| Element Name Attributes \[XML\]

deriving (Eq, Show)

*Далі наведено* приклад XML документу -- інформація про фільм
«Касабланка»:

-   Вид як у текстовому файлі

> \<film title=\"Casablanca\"\>
>
> \<director\>Michael Curtiz\</director\>
>
> \<year\>1942\</year\>
>
> \</film\>

-   Як Haskell-рядок String

> \"\<film title=\\\"Casablanca\\\"\>\\n \<director\>Michael
> Curtiz\</director\>\\n \\
>
> \\\<year\>1942\</year\>\\n\</film\>\\n\\n\\n\"

-   Текстовий рядок в Haskell може займати декілька ліній, для зручності
    читання використовуються символи з '\\' , щоб відмітити кінець одної
    лінії і її продовження на наступній

<!-- -->

-   Внутрішнє представлення (AST) -- об'єкт типу XML

> Element \"film\"
>
> \[(\"title\",\"Casablanca\")\]
>
> \[Text \"\\n \",
>
> Element \"director\" \[\] \[Text \"Michael Curtiz\"\],
>
> Text \"\\n \",

Element \"year\" \[\] \[Text \"1942\"\],

> Text \"\\n\"\]

В допоміжному файлі-заготовка, котрий включає визначення типів і даних
для тестування, надати визначення наступних функцій, котрі в сукупності
реалізують синтаксичний аналіз XML-документу.

1.  *Функція spaces s, котра пропускає всі ведучі символи проміжку в
    рядку s, повертаючи залишок рядка. Наприклад:*

    -   spaces \"\\n \\n\\nsome \\n \\n text" = \"some \\n \\n text

2.  *Функції manyN s (value s, manyT s) , котрі виділяють на початку
    рядка максимально довгий префікс, можливо порожній рядок. елементи
    якого лише символи sN (sV, sT). Функція повертає пару: префікс і
    залишок рядка. Наприклад:*

    -   *manyN "1gh.78- 1234\\n" = ("1gh.78-"," 1234\\n")*

    -   *value " bny8 \\" jio" = (" bny8 ","\\" jio")*

    -   *manyT "*\</director\>" = ("",*"*\</director\>)

3.  *Функції name s (text s, fullValue s), котрі розпізнають на початку
    рядка s лексичну одиниці name (text, fullValue). У випадку успіху
    функція повертає Just (lex, str), де lex - розпізнана лексична
    одиниця -- ім\`я (текстове поле або значення параметру-послідовність
    символів між '"") і нерозпізнаний залишок рядка str, або Nothing в
    іншому випадку. Наприклад:*

    -   *name "*year\>1942\</year\>*" = Just
        ("*year","\>1942\</year\>*")*

    -   *text "*\</year\>*" = Nothing*

    -   *fullValue "*\\"Casablanca\\"\>\\n*" = Just
        ("*Casablanca","\>\\n*")*

4.  *Функції attrib s та manyAtt s, котрі розпізнають на початку рядка
    s, відповідно, один атрибут або список атрибутів, можливо порожній.
    У випадку успіху функції повертають , відповідно, Just
    ((nm,val),str) або Just (avs, str) де (nm,val) - атрибут, avs -
    список атрибутів, str - нерозпізнаний залишок рядка. Якщо
    конструкція не розпізнана, то повертається Nothing. Наприклад:*

    -   *attrib "a = \\"scr" = Nothing*

    -   *manyAtt "x = \\\"1\\\"y = \\"2\\"\>\<b\>A" = Just
        (\[("x","1"),("y","2")\], "\>\<b\>A")*

5.  *Функції begTag s та endTag s, що розпізнають, відповідно,
    початковий і заключний теги. При успішному результаті повертають,
    відповідно, Just ((nm,avs), str) та Just (nm,str), де nm -- ім\`я
    елементу, avs - список параметрів,можливо порожній, і str -
    нерозпізнаний залишок рядка. В іншому випадку повертається -
    Nothing. Наприклад:*

    -   *begTag "\<c att=\\\"att2\\\"\> text2\</c\>" = Just
        (("c",\[("att","att2")\])," text2\</c\>")*

    -   *endTeg "\<a\>A\</a\>" = Nothing*

6.  *Взаємно рекурсивні функції element s, xml s та manyXML s, що
    розпізнають на початку рядка s відповідно один елемент, один
    компонент елементу або список компонентів, можливо порожній. У
    випадку успіху функції повертають Just (elem, str), Just (xml, str)
    або Just (xs,str), де elem, xml, xs -- розпізнані елемент, компонент
    або список компонентів, а str-- нерозпізнана частина рядка. В іншому
    випадку -- Nothing. Зауважимо, що критерій закінчення списку
    компонентів елементу -- заключний тег елементу, тобто послідовність
    символів '\<' і '/'. Мається на увазі, що manyXML st поверне (Just
    rs) тільки, коли rs містить початок заключного тегу тобто rs =
    "\</....". Це причина того, що останній з тестів наведених далі,
    повертає Nothing. Наприклад:*

    -   *element stst1 = Just (x1,"")*

    -   *element "\<a\>A\</b\>" = Nothing*

    -   *xml "\<b\>A\</b\>\<b\>B\</b\>" = Just (Element \"b\" \[\]
        \[Text \"A\"\],"\<b\>B\</b\>")*

    -   *manyXML "\<b\>\</b\>B \</a\>\" = Just (\[Element "b"\[\]\[\],
        Text "B "\]," \</a\>")*

    -   *manyXML "\<b\>\</b\>B" = Nothing*

7.  Функція *fullXML s*, котра виконує синтаксичний аналіз XML-документу
    в рядку *s*, повертаючи *Just xlm*, де *xml* -- розпізнаний
    XML-документ або *Nothing*. Наприклад:

    -   *fullXML casablanca = Just casablancaParsed*

    -   *fullXML "\<a x=\\"1\\"\>\<b\>A\</b\>\<b\>B\</b\>\</a\> x" =
        Nothing*

*spaces* :: String -\> String

*manyN, value, manyT* :: String -\> (String,String)

*name, text, fullValue* :: String -\> Maybe(String,String)

*attrib* :: String -\> Maybe ((String,String),String)

*manyAtt* :: String -\> Maybe (Attributes,String)

*begTag* :: String -\> Maybe ((String,Attributes),String)

*endTag* :: String -\> Maybe (String,String)

*element* :: String -\> Maybe (XML,String)

*xml* :: String -\> Maybe (XML,String)

*manyXML* :: String -\> Maybe (\[XML\],String)

*fullXML* :: String -\> Maybe XML

Зауваження:

Назва файлу Family10.hs (Family -- прізвище студента). Файл включає
модуль Family10 і створюється на основі файла-заготовки HWI10.hs
