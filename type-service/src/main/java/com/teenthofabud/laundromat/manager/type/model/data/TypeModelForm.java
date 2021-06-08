package com.teenthofabud.laundromat.manager.type.model.data;

import com.teenthofabud.core.common.data.form.LOVForm;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
public class TypeModelForm implements LOVForm {

    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @Getter
    @Setter
    private Long typeLovId;

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
}
