package com.teenthofabud.laundromat.manager.type.model.vo;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class TypeLOVVo implements Comparable<TypeLOVVo> {

    @ToString.Include
    private Long id;
    @ToString.Include
    private Boolean active;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;

    @Override
    public int compareTo(TypeLOVVo o) {
        return Long.compare(this.id, o.getId());
    }
}
