var ScalaMeter = (function(parent) {
  var my = { name: "data" };
  my.index = [{"scope" : ["Busqueda", "busqueda binaria"], "name" : "Busqueda binaria", "file" : "..\/Busqueda.busqueda binaria.Busqueda binaria.dsv"}, {"scope" : ["Busqueda", "busqueda binaria"], "name" : "Busqueda saltos", "file" : "..\/Busqueda.busqueda binaria.Busqueda saltos.dsv"}];
  my.tsvData = ['date	param-size	value	success	cilo	cihi	units	complete\n2020-06-29T16:10:21Z	1000	0.275326	true	-0.646	1.196	ms	"0.162647 0.390405 0.269828 0.250036 0.334733 0.272723 0.258621 0.119447 0.392648 0.302172"\n2020-06-29T16:10:21Z	2000	0.43778039999999996	true	-1.160	2.036	ms	"0.170698 0.573269 0.456316 0.420634 0.533027 0.486697 0.505122 0.152806 0.535102 0.544133"\n2020-06-29T16:10:21Z	3000	0.30674809999999997	true	-0.832	1.445	ms	"0.171011 0.352728 0.270896 0.277021 0.390077 0.26519 0.476665 0.116094 0.381488 0.366311"\n2020-06-29T16:10:21Z	4000	0.9822687999999999	true	-3.839	5.804	ms	"0.174481 1.297894 1.151078 0.856743 1.306938 1.144102 1.245473 0.121861 1.293343 1.230775"\n2020-06-29T16:10:21Z	5000	0.7188503	true	-3.341	4.779	ms	"0.192316 0.741584 0.792172 1.546221 0.695365 0.731391 0.745079 0.129776 0.886616 0.727983"\n2020-06-29T16:10:21Z	6000	1.2326118	true	-4.961	7.427	ms	"0.180491 1.589449 1.557539 1.040867 1.591023 1.495982 1.608353 0.139016 1.582103 1.541295"\n2020-06-29T16:10:21Z	7000	0.40043609999999996	true	-3.356	4.157	ms	"0.204362 0.239516 0.194592 1.255164 0.265819 0.352852 0.229634 0.152758 0.833243 0.276421"\n2020-06-29T16:10:21Z	8000	0.23651719999999998	true	-0.295	0.768	ms	"0.191952 0.231355 0.206239 0.326099 0.265145 0.196267 0.256209 0.153047 0.278397 0.260462"\n2020-06-29T16:10:21Z	9000	0.2568897	true	-0.504	1.018	ms	"0.263238 0.261376 0.181732 0.418761 0.28856 0.18225 0.264449 0.166761 0.260753 0.281017"\n2020-06-29T16:10:21Z	10000	0.26198479999999996	true	-0.539	1.063	ms	"0.248828 0.245881 0.19137 0.454253 0.264687 0.194586 0.268754 0.194298 0.290035 0.267156"\n2020-06-29T16:11:14Z	1000	0.061728000000000005	true	-0.139	0.262	ms	"0.09116 0.031864 0.050892 0.053393 0.088115 0.050749 0.070431 0.077164 0.051825 0.051687"\n2020-06-29T16:11:14Z	2000	0.24867980000000003	true	-0.677	1.174	ms	"0.298935 0.035703 0.239562 0.246457 0.286411 0.246831 0.258308 0.390942 0.239379 0.24427"\n2020-06-29T16:11:14Z	3000	0.13908550000000003	true	-0.371	0.649	ms	"0.111104 0.061318 0.144032 0.161013 0.149367 0.115627 0.135583 0.241689 0.172082 0.09904"\n2020-06-29T16:11:14Z	4000	0.7884163999999999	true	-3.693	5.270	ms	"0.624254 0.070025 0.912013 0.924383 1.122099 0.386538 0.965352 1.556137 0.92209 0.401273"\n2020-06-29T16:11:14Z	5000	1.1559864	true	-3.692	6.004	ms	"1.116382 0.050494 1.226877 1.238843 1.233318 1.105094 1.251778 1.96882 1.207422 1.160836"\n2020-06-29T16:11:14Z	6000	0.9035494	true	-5.659	7.466	ms	"0.236464 0.093074 0.421108 1.361212 1.301658 0.871116 0.689983 2.228586 0.97744 0.854853"\n2020-06-29T16:11:14Z	7000	0.5446235	true	-5.798	6.887	ms	"0.195906 0.116904 0.234875 0.11072 1.026132 1.562761 0.226233 0.131245 0.261683 1.579776"\n2020-06-29T16:11:14Z	8000	0.3646335	true	-5.250	5.979	ms	"0.200307 0.112136 0.27167 0.057985 1.849007 0.428754 0.287885 0.099877 0.263661 0.075053"\n2020-06-29T16:11:14Z	9000	0.5511247	true	-7.487	8.590	ms	"0.324829 0.073337 0.293972 0.063124 2.048072 1.928282 0.294196 0.117889 0.296773 0.070773"\n2020-06-29T16:11:14Z	10000	0.5875411000000001	true	-8.016	9.192	ms	"0.325185 0.088399 0.3217 0.071512 2.296234 1.943502 0.302819 0.133995 0.321042 0.071023"\n', 'date	param-size	value	success	cilo	cihi	units	complete\n2020-06-29T16:10:21Z	1000	0.3605663	true	-0.198	0.919	ms	"0.32623 0.334222 0.380771 0.360999 0.334964 0.347595 0.504639 0.329266 0.338818 0.348159"\n2020-06-29T16:10:21Z	2000	0.8164038	true	0.331	1.302	ms	"0.766385 0.792895 0.878691 0.87061 0.795281 0.806227 0.760801 0.795749 0.808798 0.888601"\n2020-06-29T16:10:21Z	3000	1.4111511999999997	true	0.594	2.229	ms	"1.383051 1.416382 1.483741 1.529615 1.334275 1.405941 1.319434 1.361493 1.349253 1.528327"\n2020-06-29T16:10:21Z	4000	2.6577082000000005	true	-6.152	11.467	ms	"2.416786 2.430669 2.653171 2.444691 2.141709 4.945219 1.932012 2.715345 2.277777 2.619703"\n2020-06-29T16:10:21Z	5000	7.2339731	true	-11.201	25.669	ms	"6.500792 10.31534 6.402605 6.40321 8.666255 5.358778 4.54711 9.057348 7.562383 7.52591"\n2020-06-29T16:10:21Z	6000	4.0503489	true	-0.260	8.360	ms	"3.850687 3.939417 4.503726 3.811402 4.022064 4.229351 3.435099 3.788467 4.024624 4.898652"\n2020-06-29T16:10:21Z	7000	5.080520999999999	true	-0.015	10.176	ms	"4.849295 4.828643 5.935268 4.82122 5.044746 5.122083 4.488027 4.719922 5.067652 5.928354"\n2020-06-29T16:10:21Z	8000	6.225530699999999	true	-2.358	14.809	ms	"6.22361 5.624382 6.235163 6.07906 6.044959 6.105832 5.399206 5.934945 6.189396 8.418754"\n2020-06-29T16:10:21Z	9000	6.8090949	true	4.509	9.109	ms	"6.683628 6.37258 6.707418 6.98116 7.008133 6.720977 6.733683 6.917202 7.156267 6.809901"\n2020-06-29T16:10:21Z	10000	7.5280911	true	2.202	12.854	ms	"7.459318 8.912298 7.409887 7.274005 7.482673 7.133759 7.482119 7.594324 7.185465 7.347063"\n2020-06-29T16:11:14Z	1000	0.35300539999999997	true	0.057	0.649	ms	"0.362536 0.348297 0.337194 0.32585 0.342366 0.367008 0.330752 0.421011 0.330696 0.364344"\n2020-06-29T16:11:14Z	2000	0.8614948	true	0.110	1.613	ms	"0.933687 0.900535 0.78713 0.806073 0.811713 0.878079 0.80798 0.980389 0.781003 0.928359"\n2020-06-29T16:11:14Z	3000	1.46305	true	0.559	2.367	ms	"1.600163 1.551735 1.441593 1.458476 1.38137 1.38271 1.434302 1.502085 1.332056 1.54601"\n2020-06-29T16:11:14Z	4000	2.6306353999999996	true	-2.357	7.619	ms	"2.377521 2.034573 2.404252 2.422808 3.410214 2.305964 3.26149 2.661908 2.272546 3.155078"\n2020-06-29T16:11:14Z	5000	6.770591300000001	true	-13.260	26.801	ms	"6.150076 5.118942 7.073793 6.584361 10.881326 5.036662 5.536715 9.317075 6.448883 5.55808"\n2020-06-29T16:11:14Z	6000	3.9742007	true	1.959	5.989	ms	"3.800322 3.706314 4.009412 3.880414 4.325402 3.805829 4.163845 4.123427 4.030648 3.896394"\n2020-06-29T16:11:14Z	7000	4.9130145999999995	true	3.230	6.596	ms	"4.770199 4.709232 5.038657 4.888243 4.884312 4.689822 5.110546 5.112655 5.058744 4.867736"\n2020-06-29T16:11:14Z	8000	6.1580694000000005	true	4.478	7.838	ms	"6.026608 6.045049 6.210431 6.317637 6.061038 5.948976 6.029073 6.27777 6.215341 6.448771"\n2020-06-29T16:11:14Z	9000	6.658721	true	5.126	8.192	ms	"6.445303 6.736004 6.892898 6.839024 6.623673 6.555551 6.488769 6.607017 6.644475 6.754496"\n2020-06-29T16:11:14Z	10000	7.406866500000001	true	5.377	9.437	ms	"7.230153 7.258453 7.290599 7.462321 7.803939 7.514251 7.630445 7.326814 7.219819 7.331871"\n'];
  parent[my.name] = my;
  return parent;
})(ScalaMeter || {});
