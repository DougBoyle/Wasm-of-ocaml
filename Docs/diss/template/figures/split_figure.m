labels = {'alltrees_7'; 'alltrees_9'; 'arith_{75}'; 'arith_{1000}'; 'composition'; 'funcrec'; 'mergesort_{1000}'; 'mergesort_{3000}'; 'nbody_{100}'; 'nbody_{1000}'};
data = [0.13248436103663985 0.6503574620196604 1.0; 0.3743873978996499 7.784422403733956 1.0; 0.07432675044883302 0.20323159784560144 1.0; 0.4693611205310807 0.9474982273329838 1.0; 0.062093098958333336 0.13321940104166666 1.0; 0.043206367254121664 0.2394542353610006 1.0; 0.11811963956139399 0.41146455325154707 1.0; 0.30928467377137164 1.3929310741393854 1.0; 0.2402636309167166 0.6821449970041942 1.0; 0.7511125495590257 1.3006715753701759 1.0];

errors = [0.0011170688114387846 0.022117962466487937 0.06568364611260054; 0.006592765460910152 0.05052508751458576 0.11674445740956825; 0.0025134649910233393 0.05062836624775583 0.062477558348294424; 0.017665012177451676 0.006833759801050241 0.013431163999958893; 0.000244140625 0.009521484375 0.052408854166666664; 0.0007959067652075043 0.007163160886867539 0.018874360432063673; 0.0022798827488872 0.009336662685919008 0.07045923352513299; 0.011117074292724067 0.015180556620409416 0.0770528252702599; 0.004793289394847214 0.04643499101258238 0.061713600958657876; 0.019904523019661783 0.008091269520187717 0.03454972085120155];

c = get(gca, 'DefaultAxesColorOrder');

ax1 = subplot(2, 1, 1);
bs = bar(ax1, data);
bs(1).FaceColor = c(2,:);
bs(2).FaceColor = c(3,:);
bs(3).FaceColor = c(4,:);
hold on;
ngroups = size(data, 1);
nbars = size(data, 2);
groupwidth = min(0.8, nbars/(nbars + 1.5));
for i = 1:nbars
  x = (1:ngroups) - groupwidth/2 + (2*i-1) * groupwidth / (2*nbars);
  e = errorbar(ax1, x, data(:,i), errors(:,i), '.', 'Color', 'k');
end

ax2 = subplot(2, 1, 2);
bs = bar(ax2, data);
bs(1).FaceColor = c(2,:);
bs(2).FaceColor = c(3,:);
bs(3).FaceColor = c(4,:);
hold on;
ngroups = size(data, 1);
nbars = size(data, 2);
groupwidth = min(0.8, nbars/(nbars + 1.5));
for i = 1:nbars
  x = (1:ngroups) - groupwidth/2 + (2*i-1) * groupwidth / (2*nbars);
  e = errorbar(ax2, x, data(:,i), errors(:,i), '.', 'Color', 'k');
end

%set(gca,'xtick',[1:10],'xticklabel',labels);
%legend( 'OCaml', 'OCaml GC', 'JS');
%lines = get(gca,'Children');

%title('Relative execution time');
%xlabel('Benchmark');
%set(gca,'FontSize',16);

%legend(lines([4 3 7 2 1])); 
ax1.Position = [0.1300 0.6838 0.7750 (0.3412*0.7)]
ax2.Position = [0.1300 0.1100 0.7750 (0.3412*1.5)]

an1 = annotation('line', [0.1131 0.1431], [0.6118 0.6318])

