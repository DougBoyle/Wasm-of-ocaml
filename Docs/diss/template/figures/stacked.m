labels = {'alltrees_7'; 'alltrees_9'; 'alltrees_{10}'; 'arith_{75}'; 'arith_{1000}'; 'composition'; 'funcrec'; 'mergesort_{1000}'; 'mergesort_{3000}'; 'nbody_{100}'; 'nbody_{1000}'};
%labels = {'alltrees_7'; 'arith_{75}'; 'composition'; 'funcrec'; 'mergesort_{1000}'; 'nbody_{100}'; 'pattern'};

times = [0.9804241435562806 1.4339314845024471 0.9755301794453507; 0.9526109266525807 1.5425596136432236 0.9657410202233625; 0.9454062934411728 1.5618602300012636 0.9570748557226504; 1.0699999999999998 1.375 1.1949999999999998; 1.0299136489612681 1.3358299548775738 1.1440252868824299; 1.0170827858081473 1.3600525624178712 1.0341655716162943; 1.1484375 0.9635416666666666 1.4921874999999998; 1.0624999999999998 1.4273897058823528 1.0965073529411764; 1.0445277712678696 1.2589641434262946 1.033044293414577; 0.9703337453646477 0.965389369592089 0.9826946847960445; 0.9910359695903778 0.9880857823669579 0.9895608759786678];
errors = [0.008156606851549756 0.022838499184339316 0.011419249592169658; 0.014639299728342891 0.016601267733172352 0.01131904618170842; 0.012595307300223261 0.03083533426007835 0.011794936602215764; 0.04 0.024999999999999998 0.03; 0.010650724936439223 0.010238438809867382 0.012368583797155226; 0.013140604467805518 0.023653088042049932 0.023653088042049932; 0.023437499999999997 0.010416666666666666 0.010416666666666666; 0.035845588235294115 0.058823529411764705 0.0275735294117647; 0.05366768221232716 0.0407780642137333 0.04710569486758847; 0.022249690976514212 0.019777503090234856 0.019777503090234856; 0.011687280154317484 0.014183592420288209 0.011914217633042096];

mems = [1.0 1.4695444153405908 1.0; 1.0 1.4558845766129032 1.0; 1.0 1.448985842880212 1.0; 1.0086580086580086 38.32034632034632 1.3246753246753247; 1.0006653359946773 500.83233532934133 1.332667997338656; 1.008989940370522 1.7037188765988438 1.0068278028130548; 1.625 2.3125 1.0; 1.0000463037992267 1.2787257194452806 1.0; 1.000013435472783 1.2745314938678263 1.0; 1.0 1.0 1.0; 1.0 1.0 1.0];

sizes = [1.0 1.1755994358251058 1.0; 1.0 1.1755994358251058 1.0; 1.0 1.1755994358251058 1.0; 1.0810502283105023 1.158675799086758 1.0993150684931507; 1.0810502283105023 1.158675799086758 1.0993150684931507; 1.2132736763609246 1.1263982102908277 0.9675615212527964; 0.9508076358296622 1.1395007342143906 0.9654919236417033; 1.0811785929043896 1.138304269392664 0.962717979555021; 1.0811785929043896 1.138304269392664 0.962717979555021; 1.0 1.0 1.0; 1.0 1.0 1.0];

ax1 = subplot(3,1,1);
bs = bar(ax1, times);
hold on;
%set(gca, 'YScale', 'log');
set(ax1,'xticklabel',[]);
title(ax1, 'Relative execution time');
set(ax1,'FontSize',12);
ngroups = size(times, 1);
nbars = size(times, 2);
groupwidth = min(0.8, nbars/(nbars + 1.5));
for i = 1:nbars
  x = (1:ngroups) - groupwidth/2 + (2*i-1) * groupwidth / (2*nbars);
  e = errorbar(x, times(:,i), errors(:,i), '.', 'Color', 'k');
end

ax2 = subplot(3,1,2);
bs = bar(ax2, mems);
hold on;
set(gca, 'YScale', 'log');
set(ax2,'xticklabel',[]);
title(ax2, 'Relative heap usage');
set(ax2,'FontSize',12);
ylim(ax2, [0.2 500]);

ax3 = subplot(3,1,3);
bs = bar(ax3, sizes);
hold on;
%set(gca, 'YScale', 'log');
legend(ax3, '-inline', '-uncurry', '-tailcall');
title(ax3, 'Relative file size');
set(ax3,'FontSize',12);

xlabel(ax3, 'Benchmark', 'FontSize', 10);
set(ax3,'xtick', [1:11],'xticklabel',labels);


% ax = gca
% ax.Position = 


