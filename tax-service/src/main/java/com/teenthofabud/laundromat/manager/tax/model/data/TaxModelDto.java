package com.teenthofabud.laundromat.manager.tax.model.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

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
    private Optional<String> currencyTypeModelId;
    @ToString.Include
    private Optional<String> currencyTypeModelName;


    public TaxModelDto() {
        this.name = Optional.empty();
        this.description = Optional.empty();
        this.active = Optional.empty();
        this.taxTypeModelId = Optional.empty();
        this.currencyTypeModelId = Optional.empty();
        this.currencyTypeModelName = Optional.empty();
        this.rate = Optional.empty();
    }

}
