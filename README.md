# Systems-and-methods-of-decision-making
# Тема: «Метрические	алгоритмы	классификации»

Метрические алгоритмы основываются на гипотезе компактоности, в которой сказано, что схожим объектам соответствуют схожие ответы. 

Для формализации понятия «сходства» вводится функция расстояния в пространстве объектов X.

ρ: X×X→ℝ ; ρ (X, X') - это мера близости, которая показывает насколько X похож на X'.

Метрические методы - это методы, которые основаны на анализе сходства объекстов.

Метрический алгоритм классификации  относит объект u к тому классу y , для которого суммарный вес ближайших обучающих объектов максимален.


К метрическим методам классификации относятся:
- KNN - алгоритм k ближайших соседей
- KwNN - алгоритм k взвешенных ближайших соседей
- PW - метод парзеновского окна
- PF - метод	потенциальных	функций
- STOLP - алгоритм отбора	эталонных	объектов



## Алгоритм	ближайших	соседей
### Алгоритм 	k ближайших	соседей	- KNN
Алгоритм анализа k ближайших соседей kNN - простейший метрический классификатор. Он относит классифицируемый объект к тому классу, элементов которого больше среди k ближайших соседей. 

Реализация алгоритма доступна по
[ссылке](kNN%20home.R)


![Иллюстрация к проекту](https://github.com/Pavline/Systems-and-methods-of-decision-making/blob/master/1nn%20and%20110nn.jpg)


При k = 1 этот алгоритм неустойчив к шуму. При k = ℓ, наоборот, он чрезмерно устойчив и вырождается в константу. Таким образом, крайние значения k нежелательны. На практике оптимальное значение параметра k определяют по критерию скользящего контроля с исключением объектов по одному (leave-one-out, LOO).

Суть LOO в слудующем:

1) Необходимо исключать объекты x(i) из выборки Xl по одному. Каждый раз получаем новую выборку без объекта x(i). Назовём её Xl_new.

2) Теперь будем запускать алгоритм от объекта u, который нужно классифицировать, на этой выборке Xl_new.

3) Заводим переменную Q (число ошибки, изначально Q = 0) и, когда алгоритм ошибается, увеличиваем число ошибок Q = Q + 1.

4) Когда все объекты x(i) будут перебраны, вычислить LOO = Q / l (где l - количество объектов выборки).

Для алгоритма KNN оптимальным оказалось K=6.

![Иллюстрация к проекту](https://github.com/Pavline/Systems-and-methods-of-decision-making/blob/master/6nn-loo.jpg)

Недостатком LOO является большая ресурсоёмкость, так как обучаться приходится L раз. 

Стоит заметить, что если классифицируемый объект xi не исключать из обучающей выборки, то ближайшим соседом xi всегда будет сам xi, и минимальное (нулевое) значение функционала LOO(k) будет достигаться при k = 1.

Реализация алгоритма доступна по
[ссылке](loo%20knn.R)


### Алгоритм 	k взвешенных ближайших	соседей	- KwNN
 В задачах с числом классов 3 и более  могут возникать ситуации неоднозначности. Тогда i-му соседу приписывается вес w_i, как правило, убывающий с ростом ранга соседа i. Объект относится к тому классу, который набирает больший суммарный вес среди k ближайших соседей.

![Иллюстрация к проекту](KWNN.png)
Реализация алгоритма доступна по
[ссылке](kwNN.R)


#### Недостатки простейших метрических алгоритмов типа kNN.
- Приходится хранить обучающую выборку целиком. Это приводит к неэффективному расходу памяти и чрезмерному усложнению решающего правила.
- Поиск ближайшего соседа предполагает сравнение классифицируемого объекта со всеми объектами выборки за O(ℓ) операций. Для задач с большими выборками или высокой частотой запросов это может оказаться накладно. Проблема решается с помощью эффективных алгоритмов поиска ближайших соседей, требующих в среднем O(ln ℓ) операций.
- В простейших случаях метрические алгоритмы имеют крайне бедный набор параметров, что исключает возможность настройки алгоритма по данным.


## Метод парзеновского окна
Для оценки близости объекта u к классу y алгоритм использует следующую функцию:

 ![Иллюстрация к проекту](https://github.com/Pavline/Systems-and-methods-of-decision-making/blob/master/wpw.png) , где K(z) — функция ядра (не возрастающая от 0 до бесконечности), а h — ширина окна (окно — сферическая окрестность классифицируемого объекта u радиуса h).

Чаще всего применяются 5 типов ядер:

![Иллюстрация к проекту](https://github.com/Pavline/Systems-and-methods-of-decision-making/blob/master/ker.png)

![Иллюстрация к проекту](https://github.com/Pavline/Systems-and-methods-of-decision-making/blob/master/pw%20r%20t%20q%20e.jpg)

![Иллюстрация к проекту](https://github.com/Pavline/Systems-and-methods-of-decision-making/blob/master/pw%20gauss.png)

Выбор ядра слабо влияет на качество классификации. 

Преимущества алгоритма: 
- хорошее качество классификации при правильно подобраном h
- все точки с одинаковым расстоянием будут учитаны

Недостатки:
- необходимо хранить всю выборку целиком
- диапазон параметра h необходимо подбирать самостоятельно, учитывая плотность расположения точек
- если ни одна точка не попала в радиус h, алгоритм не способен ее классифицировать (не актуально для гауссовского ядра)

Реализация алгоритма доступна по
[ссылке](pw.R)


## Метод	потенциальных	функций

Если в методе парзеновского окна центр окна поместить в классифицируемый
объект, то получим метод потенциальных функций.

##  Отбор	эталонных	объектов


# Тема:	«Байесовские	алгоритмы	классификации»

## Линии	уровня	нормального	распределения

## Наивный	нормальный	байесовский	классификатор
Наивный байесовский классификатор (naїve Bayes) — специальный частный случай байесовского классификатора, основанный на дополнительном предположении, что объекты x  описываются n статистически независимыми признаками:

![Иллюстрация к проекту](https://github.com/Pavline/Systems-and-methods-of-decision-making/blob/master/nb1.png)

Предположение о независимости означает, что функции правдоподобия классов представимы в виде

![Иллюстрация к проекту](https://github.com/Pavline/Systems-and-methods-of-decision-making/blob/master/nb2.png)

где  ![Иллюстрация к проекту](https://github.com/Pavline/Systems-and-methods-of-decision-making/blob/master/nb3.png) — плотность распределения значений  j-го признака для класса y.

Предположение о независимости существенно упрощает задачу, так как оценить n одномерных плотностей гораздо легче, чем одну n-мерную плотность. К сожалению, оно крайне редко выполняется на практике, отсюда и название метода.

Наивный байесовский классификатор может быть как параметрическим, так и непараметрическим, в зависимости от того, каким методом восстанавливаются одномерные плотности.

Основные __преимущества__ наивного байесовского классификатора:
- Простота реализации.
- Низкие вычислительные затраты при обучении и классификации.
- В тех редких случаях, когда признаки действительно независимы (или почти независимы), наивный байесовский классификатор (почти) оптимален.

Основной его __недостаток__ — относительно низкое качество классификации в большинстве реальных задач.

Чаще всего он используется либо как примитивный эталон для сравнения различных моделей алгоритмов, либо как элементарный строительный блок в алгоритмических композициях.

![Иллюстрация к проекту](https://github.com/Pavline/Systems-and-methods-of-decision-making/blob/master/nbc.png)

Реализация алгоритма доступна по
[ссылке](nbc.R)

## Подстановочный	алгоритм	(plug-in)

## Линейный	дискриминант	Фишера	– ЛДФ

## Сеть	радиальных	базисных	функций	– RBF сеть

# Тема	«Линейные	алгоритмы	классификации»
## ADALINE.	Правило	Хэбба	(персептрон	Розенблатта)

## Логистическая	регрессия

## Метод	опорных	векторов	– SVM.	ROC-кривая
