Decay gamma sampling procedure
================================
Sampling of decay gammas is implemented differently in the new (this) and 
the original R2Smesh. Both methods are described here and compared.

Original R2Smesh
-------------------
The ``source.tNN`` file contains information about decay gamma intensity and spectrum for
all fine mesh elements (for all fine mesh elements with non-zero decay gamma intensity).
Particularly, for each fine mesh element with activated material, this file contains a line
containing the mesh element center coordinates, followed by intensity of gammas in each 
energy group normalized to material volume, ``S_gi``, where ``g = 1 .. G`` is the energy group index and
``i = 1 .. N`` is the fine mesh element index. ``G`` is the number of energy groups used to represent 
decay gamma spectrum and ``N`` is the number of fine mesh elements with non-zero gamma intensity.

The following equality holds::

  (1)       sum( S_gi, g=1..G) * v_i * f_i = I_i

where ``v_i`` is the fine mesh element volume, ``f_i`` is the volume fraction of 
all materials in the fine mesh element and ``I_i`` is the gamma intensity (gammas per second) 
emitted from this fine mesh element. The ``intensity.tNN`` files contain ``I_i`` values.

The ``source.tNN`` file is read into MCNP in the ``source.F90`` subroutine before sampling the 1-st 
source. A fine mesh element is sampled uniformly from ``N`` and the source coordinate is sampled 
uniformly in the sampled fine mesh element. The coordinate is accepted only if it hits a non-void cell, 
otherwise another fine mesh element is sampled. Since after rejecting source in void a new
mesh element is sampled, the probability ``P_i`` to sample source from i-th mesh element is proportional
to the volumetric fraction of material in that mesh element::

  (2)       P_i = f_i / sum(f_j, j=1..N)

The source particle weight ``W_i`` in the i-th fine mesh element is chosen to ensure the element's intensity ``I_i``:: 

  (3)       P_i W_i = I_i
  
Expressing from this equality ``W_i`` and inserting expressions for ``P_i`` and ``I_i`` we obtain::

  (4)       W_i  =  I_i / P_i  
                 =  sum(S_gi, g) v_i f_i sum(f_j, j) / f_i  
                 =  sum (S_gi, g) V f
  
where ``V = v N`` is the total volume of all emitting mesh elements and ``f = sum(f_j, j)/N`` is the volumetric fraction 
of materials in the emitting mesh elements.


This implementation
---------------------
The ``dgs.NN`` file contains gamma intensity ``S_gi`` for each fine mesh 
element ``i`` and each gamma energy group ``g``. Note that here the values are NOT normalized to material volume, thus
the total gamma intensity in i-th mesh element is::

  (5)       sum(S_gi, g=1..G)  =  I_i

(compare to eq. (1) above). Values ``S_gi`` are used in in the source sampling subroutine. First, the fine mesh element is
chosen randomly and homogeneously from ``1 .. N``. At the next step, a coordinate is sampled within the chosen mesh 
element untill it hits a non-void cell. Thus, the probability to sample coordinate in i-th mesh element is::
 
  (6)       P_i = 1/N
  
Note the difference with the original implementation, where a new mesh element is sampled when a void cell is hit. Here, sampling
of the coordinate is repeated for the same mesh element untill it hits a non-void cell. This can be inefficient for a mesh 
element with small vol. fraction of material. 

The source particle weight ``W_i`` is obtained by inserting (5) and (6) to (3)::

  (7)       W_i  =  I_i / P_i
                 =  N sum(S_gi, g=1..G)
                 
                 
Comparison
------------
The original implementation might appear more effective. Consider a mesh element with small material volumetric fraction. In the original implementation, when the chosen coordinate does not hit material, another element (probably with larger material volumetric fraction and thus more probability to be accepted) is sampled. In the new implementation, a new coordinate is sampled in the same mesh element until material is hit.

For proper weight normalization, the original implementation needs additional information about the material volumetric fractions. It is supplied implicitly, while the decay gamma source file contains gamma intensity normalized to material volume. More straightforward way is used in the new implementation, where the values in the decay gamma source file are total intensities in each mesh element and can be directly used to e.g. compute the total gamma intensity (simple sum of all entries) in the region covered by the mesh.

Drawbacks
-----------
Rejection of decay gamma source coordinates in void cells can be insufficient in some configurations. Consider a steel pipe filled with water. Steel is highly activated as compared to water. In this configuration, gammas will be emmitted both from the pipe walls (as it should be) and from water (since this is not void region). The latter is wrong (decay gamma is originated from steel, not from water) and can affect local distribution of decay heat and SDDR.
