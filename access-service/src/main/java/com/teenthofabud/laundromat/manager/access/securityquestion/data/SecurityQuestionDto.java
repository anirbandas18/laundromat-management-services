package com.teenthofabud.laundromat.manager.access.securityquestion.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class SecurityQuestionDto {

    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> active;


    public SecurityQuestionDto() {
        this.name = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
