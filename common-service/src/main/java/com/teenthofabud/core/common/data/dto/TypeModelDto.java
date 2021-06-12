package com.teenthofabud.core.common.data.dto;

import lombok.*;

import java.util.Optional;

@Getter
@Setter
@AllArgsConstructor
@ToString
@EqualsAndHashCode
public class TypeModelDto {
    @ToString.Include
    @EqualsAndHashCode.Include
    private Optional<String> name;
    @ToString.Include
    @EqualsAndHashCode.Include
    private Optional<String> id;
    public TypeModelDto() {
        this.id = Optional.empty();
        this.name = Optional.empty();
    }
}