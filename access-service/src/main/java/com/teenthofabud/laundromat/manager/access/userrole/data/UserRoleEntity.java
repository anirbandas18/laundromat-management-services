package com.teenthofabud.laundromat.manager.access.userrole.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.laundromat.manager.access.role.data.RoleEntity;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Entity
@Table(name = "user_role")
public class UserRoleEntity extends TOABBaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @ToString.Include
    private Long id;
    /*@EmbeddedId
    private PermissionMap permissionMapping;*/
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    //@MapsId("userId")
    @JoinColumn(name = "user_model_id")
    private UserTypeEntity userType;
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    //@MapsId("operationId")
    @JoinColumn(name = "role_model_id")
    private RoleEntity role;

}
