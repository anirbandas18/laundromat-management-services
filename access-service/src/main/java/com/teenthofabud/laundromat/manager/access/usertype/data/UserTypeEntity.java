package com.teenthofabud.laundromat.manager.access.usertype.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionEntity;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleEntity;
import lombok.*;

import javax.persistence.*;
import java.util.Set;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Entity
@Table(name = "user_model")
public class UserTypeEntity extends TOABBaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @ToString.Include
    private Long id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @OneToMany(mappedBy = "userType", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<UserRoleEntity> userRoles;

}
