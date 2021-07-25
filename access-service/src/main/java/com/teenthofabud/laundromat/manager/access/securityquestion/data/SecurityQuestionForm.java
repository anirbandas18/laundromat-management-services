package com.teenthofabud.laundromat.manager.access.securityquestion.data;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.teenthofabud.core.common.data.form.LOVForm;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import lombok.ToString;

@NoArgsConstructor
@AllArgsConstructor
@ToString
public class SecurityQuestionForm implements LOVForm {

    @ToString.Include
    private String name;

    @Override
    public void setName(String name) {
        this.name = name;
    }

    @JsonIgnore
    @Override
    public void setDescription(String description) {
        throw new UnsupportedOperationException("setting Description attribute is logically not applicable for SecurityQuestionForm");
    }

    @Override
    public String getName() {
        return this.name;
    }

    @JsonIgnore
    @Override
    public String getDescription() {
        throw new UnsupportedOperationException("getting Description attribute is logically not applicable for SecurityQuestionForm");
    }
}
