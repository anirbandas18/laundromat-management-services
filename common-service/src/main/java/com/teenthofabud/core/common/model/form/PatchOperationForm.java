package com.teenthofabud.core.common.model.form;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class PatchOperationForm {

    private String op;
    private String path;
    private String value;

}
