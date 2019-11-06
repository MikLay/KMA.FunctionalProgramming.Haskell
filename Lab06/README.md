Робота №6
---------

*Екземпляри класів типів*

В цій роботі будуть розглядатися поліноми, у яких коефіцієнти довільні
числа (дані що являються екземплярами класу Num). Поліноми можна
додавати, віднімати і множити, схожим чином як інші *числа*. Головна
мета цієї роботи - зробите поліноміальний тип екземпляром класу Num.

Поліном - це послідовність термів, кожний з яких має *коефіцієнт* і
*степінь*. Послідовність термів можна представити списком чисел --
коефіцієнтів, кожний з яких має степінь, котра співпадає з його позицією
в списку.

> *Перший елемент списку має позицію 0.*
>
> *Поліноми x^2^+5x+3 і 5x^3^+2 можна представити , відповідно, списками
> \[3, 5, 1\] і \[2, 0, 0,5\].*

Для представлення поліномів в Haskell вводиться тип

> ***newtype** Poly a = P \[a\]*

Ім'я типу -- *Poly,* кожний елемент цього типу має одне поле -- список
чисел і будується за допомогою конструктора *P.*

> *Поліноми x^2^+5x+3 і 5x^3^+2 тоді запишуться як P \[3,5,1\] і P
> \[2,0,0,5\].*

Два поліноми -- дані типу Poly a -- можуть бути рівні, а списки що їх
представляють -- ні!

Краще для користувача, якщо поліном 3x^2^+x2+1 буде відображатися не як
P \[1, 2,3\], а як 3x\^2 + 2x +1.

Тому тип Poly a не можна зробити екземпляром класу Eq або Show
використовуючи deriving.

Відображення поліному (екземпляр класу Show) повинно мати наступні
характеристики:

-   Терми відображаються як cx\^e де c - коефіцієнт і e -- степінь. Якщо
    c є 0 , то відображається лише коефіцієнт. Якщо e є 1, то формат
    просто cx.

-   Терми розділяються знаком + з єдиним проміжком з кожного боку.

-   Терми розміщуються по мірі *зменшення* степені.

-   Ніщо не відображується для термів, котрі мають коефіцієнт 0, якщо
    тільки поліном не рівний 0.

-   Не відображається коефіцієнт для терму, що має коефіцієнт 1, за
    винятком коли його степінь 0, тобто x замісто 1x.

-   Непотрібно якось спеціально обробляти терми, що мають від'ємні
    коефіцієнти. Наприклад, для 2x^2^ - 3 його вірне представлення має
    вид 2x\^2 + -3.

Поліноми можна обчислювати і диференціювати.

Оскільки поліноми не єдиний тип математичних функцій, котрий може бути
диференційованим, то визначається новий клас типів Differentiable.

> ***class*** Num a =\> Differentiable a ***where***
>
> derive :: a -\> a
>
> nderive :: Int -\> a -\> a

Дві функції в класі типів Differentiable -- це derive (перша похідна) і
nderive (n-похідна) свого аргументу. Екземпляри цього класу типів
повинні задовольняти наступні закони:

-   ∀n\>0, nderiv (n-1) (derive f) == nderiv n f

-   ∀n\>0, derive ( nderiv (n-1) f) == nderiv n f

Можна надати реалізацію nderiv за замовчуванням в об»яві класу типів
Differentiable, в термінах derive. Значення nderiv n f дорівнює
результату застосування derive n разів.

На основі допоміжного файлу, котрий включає визначення типів, допоміжних
функцій і даних для тестування, створити файл, в якому надати визначення
наступних функцій та екземплярів класів типів.

1.  Функція-константа *x*, котра завжли будує це поліном f(x) = x*.*

2.  Встановити тип *Poly a* екземпляром класу типів *Eq*, реалізувавши
    функцію (==).

3.  Встановити тип *Poly a* екземпляром класу типів *Show*, реалізувавши
    функцію *show* згідно вказаним характеристикам.

4.  Функція *plus p1 p2*, котра додає поліноми *p1* і *p2* (два об»єкта
    типа Poly a). Наприклад:

    -   plus (P \[5, 0, 1\]) (P \[1, 1, 2\]) = P \[6, 1, 3\]

5.  Функція *times* *p1 p2*, котра множить поліноми *p1* і *p2* (два
    об»єкта типа Poly a). Наприклад:

    -   times (P \[1, 1, 1\]) (P \[2, 2\] ) = P \[2, 4, 4, 2\]

6.  Встановіть тип *Poly a* екземпляром класу типів *Num*. Функції abs і
    signum можна не визначати, залишивши undefined, тому що абсолютна
    величина полінома -- НЕ поліном і поліном не має знаку

7.  Функція *applyP p x*, котра обчислює значення полінома *p* (об"єкт
    типа Poly a) в точці *x* (об»єкт типа a). Наприклад:

    -   applyP (P \[1, 2, 1\]) 1 = 4

8.  Закінчіть визначення класу типів Differentiable. Визначте функцію
    nderiv в термінах derive. Значення nderiv n f дорівнює результату
    застосування derive n разів.

9.  Встановити тип *Poly a* екземпляром класу типів *Differentiable*,
    реалізувавши функцію *derive*.

*x* :: Num a =\> Poly a\]

*plus* :: Num a =\> Poly a -\> Poly a -\> Poly a

*times* :: Num a =\> Poly a -\> Poly a -\> Poly a

*applyP* :: Num a =\> Poly a -\> a -\> a

Зауваження:

Назва файлу Family06.hs (Family -- прізвище студента). Файл включає
модуль Family06 і створюється на основі файла-заготовки HWI06.hs