package com.teenthofabud.laundromat.manager.type.model.dto;

import lombok.*;

import java.util.Optional;

@Getter
@Setter
@ToString
@NoArgsConstructor
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

}
