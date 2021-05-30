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
        this.name = Optional.empty();
        this.typeLovId = Optional.empty();
        this.description = Optional.empty();
        this.active = Optional.empty();
    }

}
