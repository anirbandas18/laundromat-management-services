package com.teenthofabud.laundromat.manager.tax.model.data;

import com.teenthofabud.core.common.data.form.LOVForm;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
public class TaxModelForm implements LOVForm {

    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @Getter
    @Setter
    @ToString.Include
    private Float rate;
    @Getter
    @Setter
    @ToString.Include
    private TypeModelForm currencyTypeModelForm; //  pojo in nature to store both id and client friendly name, turnover rate is negligible
    @Getter
    @Setter
    @ToString.Include
    private Long taxTypeModelId; //  absolute value in nature for business logic reference, turnover rate is relatively high

    @Override
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public void setDescription(String description) {
        this.description = description;
    }

    @Override
    public String getName() {
        return this.name;
    }

    @Override
    public String getDescription() {
        return this.description;
    }

    @Getter
    @Setter
    @AllArgsConstructor
    @ToString
    @EqualsAndHashCode
    public static final class TypeModelForm {
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
}
