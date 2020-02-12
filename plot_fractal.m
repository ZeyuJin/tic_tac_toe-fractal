%% This script provided by Peter Shearer
%% Modified by Zeyu Jin

clf reset
clear
[fid, message] = fopen('julia.bin');

%% Fortran I/O has extra four bytes at the beginning and end of writes
% read the number of slides
[dum, count] = fread(fid, [1], 'int');
[num_rot, count] = fread(fid, [1], 'int');
[dum, count] = fread(fid, [1], 'int');


[dum, count] = fread(fid, [1], 'int');
[size, count] = fread(fid, [2], 'int');
[dum, count] = fread(fid, [1], 'int');

nx = size(1);
ny = size(2);

[dum, count] = fread(fid, [1], 'int');
[boundary, count] = fread(fid, [4], 'float');
[dum, count] = fread(fid, [1], 'int');

x1 = boundary(1);
x2 = boundary(2);
y1 = boundary(3);
y2 = boundary(4);

[dum, count] = fread(fid, [1], 'int');
[z, count] = fread(fid, [num_rot, nx*ny], 'float');
fclose(fid);

z2 = log10(z);
axis([x1, x2, y1, y2]); axis('square'); hold on;

% create animated GIF images
h = figure;
axis tight manual
filename = 'julia.gif';
for i = 1:num_rot
    z_plot = z2(i,:);
    z_plot = reshape(z_plot, [ny,nx]);
    imagesc([x1, x2], [y1, y2], z_plot);
    colormap(jet);
    colorbar;
    caxis([0, 2]);
    drawnow
    
    % Capture the plot as an image 
    frame = getframe(h); 
    im = frame2im(frame); 
    [imind,cm] = rgb2ind(im,256); 
    % Write to the GIF File 
    if i == 1 
        imwrite(imind, cm, filename, 'gif', 'Loopcount', inf); 
    else 
        imwrite(imind, cm, filename, 'gif', 'WriteMode', 'append');
    end
end