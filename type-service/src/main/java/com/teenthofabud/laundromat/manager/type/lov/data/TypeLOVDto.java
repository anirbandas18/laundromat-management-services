package com.teenthofabud.laundromat.manager.type.lov.data;

import lombok.*;

import java.util.Optional;

@Getter
@Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TypeLOVDto {

    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> description;
    @ToString.Include
    private Optional<String> active;

}
