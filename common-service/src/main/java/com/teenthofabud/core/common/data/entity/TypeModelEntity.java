package com.teenthofabud.core.common.data.entity;

import lombok.*;

import javax.persistence.Embeddable;

@Getter
@Setter
@AllArgsConstructor
@ToString
@EqualsAndHashCode
@Embeddable
public class TypeModelEntity {
    @ToString.Include
    @EqualsAndHashCode.Include
    private String name;
    @ToString.Include
    @EqualsAndHashCode.Include
    private Long id;
    public TypeModelEntity() {
        this.id = 0L;
        this.name = "";
    }
}