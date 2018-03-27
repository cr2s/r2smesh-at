"""
Extract spectra from mesh tally.
"""
from mcrp_splitters import meshtal_read
from mcrp_splitters.meshtal import tonumpy, crop, integrate, pprint, indexing


def get_bounds(cla):
    """
    cla is the command line argument of the form

    "x1 x2, y1 y2, z1 z2" or
    "x, y, z" or

    any mix of them.

    """
    bounds = tuple(map(float, d.split()) for d in cla.split(','))
    bounds = ((min(b), max(b)) for b in bounds)
    return bounds


def get_meshtal(f):
    """
    Return first tally with energy segmenting from meshtal file f.
    """
    # Scan meshtal for the 1-st tally with energy bins
    tlst = []
    for m in meshtal_read(f, readvalues=False):
        bins = m[0]
        npc = m[-1]
        if 't' in bins['e']:
            tlst.append(npc[0])

    for m in meshtal_read(f, tally=tlst):
        yield m


if __name__ == '__main__':
    from sys import argv
    b = {}
    b['x'], b['y'], b['z'] = get_bounds(argv[1])
    bounds = b
    for k, v in b.items():
        print '{} boundaries:'.format(k), v

    for m in get_meshtal(argv[2]):
        print '*' * 80
        bins, val, h, probid, tit, nps, (n, p, c) = m
        a = tonumpy(bins, val, h)
        print 'Original mesh'
        pprint(bins, a, h)
        new_bins, b = crop(bins, val, h, bounds=bounds)
        print 'Cropped mesh'
        pprint(new_bins, b, h)

        for d in bounds.keys():
            new_bins, b = integrate(new_bins, b, h, d, average=True)
            print 'After integration along', d
            pprint(new_bins, b, h)

        # print out integrated over spatial dimension spectrum
        i = h.index('e')
        eb = new_bins['e']
        for j in range(b.shape[i]):
            e1 = eb[j]
            e2 = eb[j + 1]
            vvv = indexing(b, h, e=slice(j, j+1, 1)).flatten()
            svals = str(vvv)
            # svals = '{:15.8e} {:10.7f}'.format(val, err)
            if e2 == 't':
                srng = '{:>34s}: '.format('total')
            else:
                srng = '{:15.8e} -- {:15.8e}: '.format(e1, e2)
            print srng + svals
