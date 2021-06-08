package com.teenthofabud.core.common.data.vo;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class ErrorVo {

    @ToString.Include
    private String code;
    @ToString.Include
    private String message;
    @ToString.Include
    private String domain;

}
