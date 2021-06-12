package com.teenthofabud.core.common.data.vo;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class TypeModelVo {
    private Long id;
    private String name;
    public TypeModelVo() {
        this.id = 0L;
        this.name = "";
    }
}
