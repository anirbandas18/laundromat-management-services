package com.teenthofabud.laundromat.manager.tax.model.data;

import com.teenthofabud.core.common.data.dto.TypeModelDto;
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
    private Optional<String> taxLovId;
    @ToString.Include
    private Optional<TypeModelDto> currencyTypeModel;

    public TaxModelDto() {
        this.name = Optional.empty();
        this.description = Optional.empty();
        this.active = Optional.empty();
        this.taxLovId = Optional.empty();
        this.rate = Optional.empty();
        this.currencyTypeModel = Optional.of(new TypeModelDto());
    }

}
