package com.teenthofabud.laundromat.manager.access.usertype.data;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class UserTypeVo implements Comparable<UserTypeVo> {

    @ToString.Include
    private Long id;
    @ToString.Include
    private Boolean active;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;

    @Override
    public int compareTo(UserTypeVo o) {
        return this.id.compareTo(o.getId());
    }
}
