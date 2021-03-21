%labels = {'alltrees_7'; 'alltrees_9'; 'arith_{75}'; 'arith_{1000}'; 'composition'; 'funcrec'; 'mergesort_{1000}'; 'mergesort_{3000}'; 'nbody_{100}'; 'nbody_{1000}'};
labels = {'alltrees'; 'arith'; 'composition'; 'funcrec'; 'mergesort'; 'nbody'};

data = [8207.0 1418.0 14371.0 4982.0; 984.0 876.0 13676.0 4436.0; 11314.0 2682.0 15021.0 10874.0; 1351.0 1362.0 14128.0 7033.0; 8950.0 1663.0 29286.0 8690.0; 4145.0 4399.0 15381.0 22977.0];
data = data/1024;

%errors = [0 0 84726.326 0; 0 0 1670782.392 0; 0 0 2554.84 0; 0 0 3051.923 0; 0 0 420772.029 0; 0 0 2939.434 0; 0 0 347349.408 0; 0 0 791446.291 0; 0 0 2480.216 0; 0 0 107762.219 0];

data2 = [0 1796.0 0 0; 0 1248.0 0 0; 0 4337.0 0 0; 0 1908.0 0 0; 0 2293.0 0 0; 0 5408.0 0 0];
data2 = data2/1024;
%errors2 = [0 0 0 0; 0 0 0 0; 0 0 0 0; 0 0 0 0; 0 0 0 0; 0 0 0 0; 0 0 0 0; 0 0 0 0; 0 0 0 0; 0 0 0 0];


c = get(gca, 'DefaultAxesColorOrder');
bar(data2, 'FaceColor', c(3,:))
hold on;
bs = bar(data);
bs(1).FaceColor = c(1,:);
bs(2).FaceColor = c(2,:);
bs(3).FaceColor = c(4,:);
bs(4).FaceColor = c(5,:);

%set(gca, 'YScale', 'log');
set(gca,'xtick',[1:10],'xticklabel',labels);
legend('', 'OCaml GC', '', '', 'C', 'OCaml', 'JS', 'Grain');
lines = get(gca,'Children');


title('Output file size');
ylabel('Size (KB)');
%ylim([20   100000000]);
xlabel('Benchmark');
set(gca,'FontSize',16);

%ngroups = size(data, 1);
%nbars = size(data, 2);
%groupwidth = min(0.8, nbars/(nbars + 1.5));
%i = 2;
%x = (1:ngroups) - groupwidth/2 + (2*i-1) * groupwidth / (2*nbars);
%er = errorbar(x, data2(:,i), errors2(:,i), '.', 'Color', 'k');

%for i = 1:nbars
%  x = (1:ngroups) - groupwidth/2 + (2*i-1) * groupwidth / (2*nbars);
%  e = errorbar(x, data(:,i), errors(:,i), '.', 'Color', 'k');
%end

legend(lines([4 3 7 2 1])); 


% ax = gca
% ax.Position = 


