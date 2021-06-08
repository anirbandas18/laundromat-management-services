package com.teenthofabud.core.common.data.form;

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
