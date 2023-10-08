Для корректной работы тестов необходимо ввести данные в файлах:
 - /priv/conn.conf
		указать порт: port UInt
 - /test/ct_dir/uctest_SUITE.erl
		в init_per_suite указать абсолютный путь до папки /priv
 - /test/ct_dir/cover.spec
		пути до исходников приложения /src и откомпилированных с опцией debug_info файлов /ebin
 - /test/ct_dir/uctest.spec
		папку с файлами для теста и папку для сранья логами

Тестированию подвергаются ca:ca , генератор чисел , satel:handler , фильтр простых чисер, использующий тест Миллера-Рабина.

ct:run_test([{spec, "uctest.spec"}])