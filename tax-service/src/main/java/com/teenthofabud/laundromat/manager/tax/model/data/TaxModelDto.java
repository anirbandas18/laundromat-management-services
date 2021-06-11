package com.teenthofabud.laundromat.manager.tax.model.data;

import lombok.*;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class TaxModelDto {

    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> description;
    @ToString.Include
    private Optional<String> active;
    @ToString.Include
    private Optional<String> rate;
    @ToString.Include
    private Optional<String> taxTypeModelId;
    @ToString.Include
    private Optional<TypeModelDto> currencyTypeModel;


    @Getter
    @Setter
    @AllArgsConstructor
    @ToString
    @EqualsAndHashCode
    public static final class TypeModelDto {
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

    public TaxModelDto() {
        this.name = Optional.empty();
        this.description = Optional.empty();
        this.active = Optional.empty();
        this.taxTypeModelId = Optional.empty();
        this.rate = Optional.empty();
        this.currencyTypeModel = Optional.of(new TypeModelDto());
    }

}
