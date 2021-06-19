package com.teenthofabud.laundromat.manager.type.model.data;

import lombok.*;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class TypeModelDto {

    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> typeLovId;
    @ToString.Include
    private Optional<String> description;
    @ToString.Include
    private Optional<String> active;

    public TypeModelDto() {
        this.name = Optional.ofNullable(null);
        this.typeLovId = Optional.ofNullable(null);
        this.description = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
