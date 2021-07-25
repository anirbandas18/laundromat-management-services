package com.teenthofabud.laundromat.manager.access.securityquestion.data;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class SecurityQuestionVo implements Comparable<SecurityQuestionVo> {

    @ToString.Include
    private Long id;
    @ToString.Include
    private Boolean active;
    @ToString.Include
    private String name;

    @Override
    public int compareTo(SecurityQuestionVo o) {
        return this.id.compareTo(o.getId());
    }
}
