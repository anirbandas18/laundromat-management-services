package com.teenthofabud.core.common.data.form;

import lombok.*;

@Getter
@Setter
@AllArgsConstructor
@ToString
@EqualsAndHashCode
public class TypeModelForm {
    @ToString.Include
    @EqualsAndHashCode.Include
    private String name;
    @ToString.Include
    @EqualsAndHashCode.Include
    private Long id;
    public TypeModelForm() {
        this.id = 0L;
        this.name = "";
    }
}