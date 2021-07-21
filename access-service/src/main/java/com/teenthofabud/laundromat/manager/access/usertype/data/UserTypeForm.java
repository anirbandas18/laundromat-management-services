package com.teenthofabud.laundromat.manager.access.usertype.data;

import com.teenthofabud.core.common.data.form.LOVForm;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import lombok.ToString;

@NoArgsConstructor
@AllArgsConstructor
@ToString
public class UserTypeForm implements LOVForm {

    @ToString.Include
    private String name;
    @ToString.Include
    private String description;

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
