package com.teenthofabud.laundromat.manager.tax.integration.type.data;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class TypeModelVo implements Comparable<TypeModelVo> {

    @ToString.Include
    private Long id;
    @ToString.Include
    private Boolean active;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private TypeLOVVo typeLov;

    @Override
    public int compareTo(TypeModelVo o) {
        return Long.compare(this.id, o.getId());
    }
}
