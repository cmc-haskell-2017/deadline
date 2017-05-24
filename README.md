# Deadline
Игра «Deadline», бесконечный платформер. Работа сделана студентами 325 группы, Ким Джун Мюн, Пархоменко Екатерина, Лихтарова Анна, Степанян Нарек в рамках курса по языку Haskell ВМК МГУ. В частности, в данной ветви для индивидуальной части рассматривается взаимодействие с базой данных.

# Инструкция для пользования
Для того чтобы двигать персонала воспользуйтесь кнопками вправо, влево
Для того чтобы начать новую игру и увидеть свой балл в текущей игре и таблицу всех рекордов, нажмите пробел. В таблице рекордов отображается первые 10 топ рекорды по убыванию.
По окончанию игры ваши баллы автоматически сохраняются.

# База Данных
Данная индивидуальная часть реализована студентов Ким Джун Мюн. Для реализации базы данных использован SQLite. В частности, библиотека SQLite.Simple для удобства. 

Из пунктов рекомендуемых для реализации выбраны 

1) Таблица рекордов 
2) Таблица пользователей

Все функции, которые связаны с базой данных, собраны в модуле «Database».

Функции делятся в 4 категории:

1) Создание таблицы бд
2) Создание записи в бд
3) Обновление данных в бд
4) Запрос данных из бд
