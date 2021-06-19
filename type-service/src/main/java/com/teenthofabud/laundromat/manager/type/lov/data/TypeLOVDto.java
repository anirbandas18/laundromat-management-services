package com.teenthofabud.laundromat.manager.type.lov.data;

import lombok.*;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class TypeLOVDto {

    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> description;
    @ToString.Include
    private Optional<String> active;

    public TypeLOVDto() {
        this.name = Optional.ofNullable(null);
        this.description = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
