package com.teenthofabud.laundromat.manager.access.rolepermission.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionException;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionForm;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionMessageTemplate;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionVo;
import com.teenthofabud.laundromat.manager.access.rolepermission.service.RolePermissionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Set;


@RestController
@RequestMapping("rolepermission")
@Slf4j
@Tag(name = "RolePermission API", description = "Manage RolePermissions and their details")
public class RolePermissionController {

    private static final String MEDIA_ACCESS_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(RolePermissionService service) {
        this.service = service;
    }

    private RolePermissionService service;

    @Operation(summary = "Create new RolePermission details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created RolePermission",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = String.class)) }),
            @ApiResponse(responseCode = "400", description = "RolePermission attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "RolePermission already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No RolePermission attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new RolePermission",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewRolePermission(@RequestBody(required = false) RolePermissionForm form) throws RolePermissionException {
        log.debug("Requesting to create new rolePermission");
        if(form != null) {
            Long id = service.createRolePermission(form);
            log.debug("Responding with identifier of newly created new rolePermission");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("RolePermissionForm is null");
        throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update RolePermission details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of RolePermission",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "RolePermission attribute's value is invalid/Role is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No RolePermission found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "RolePermission already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update RolePermission details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingRolePermission(@PathVariable String id, @RequestBody(required = false) RolePermissionForm form) throws RolePermissionException {
        log.debug("Requesting to update all attributes of existing rolePermission");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_VALID.getValue(), id);
                if(form != null) {
                    service.updateRolePermission(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing rolePermission");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("RolePermissionForm is null");
                throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_INVALID.getValue(), id);
                throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_EMPTY.getValue());
        throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete RolePermission by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted RolePermission",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "RolePermission id is invalid/Role is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No RolePermission found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No RolePermission attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete RolePermission",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingRolePermission(@PathVariable String id) throws RolePermissionException {
        log.debug("Requesting to soft delete rolePermission");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_VALID.getValue(), id);
                service.deleteRolePermission(actualId);
                log.debug("Responding with successful deletion of existing rolePermission");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_INVALID.getValue(), id);
                throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_EMPTY.getValue());
        throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch RolePermission attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of RolePermission with the given value",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "RolePermission attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No RolePermission found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No RolePermission attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of RolePermission with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_ACCESS_APPLICATION_JSON_PATCH)
    public ResponseEntity<Void> patchExistingRolePermission(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws RolePermissionException {
        log.debug("Requesting to patch of rolePermission attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_VALID.getValue(), id);
                if(dtoList != null) {
                    service.applyPatchOnRolePermission(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing rolePermission");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("rolePermission patch document is null");
                throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_INVALID.getValue(), id);
                throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_EMPTY.getValue());
        throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all RolePermission details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available RolePermissions and their details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = RolePermissionVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<RolePermissionVo> getAllRolePermissionNaturallyOrdered() {
        log.debug("Requesting all available rolePermissions by their natural orders");
        Set<RolePermissionVo> naturallyOrderedRolePermissions = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available rolePermissions by their natural orders");
        return naturallyOrderedRolePermissions;
    }

    @Operation(summary = "Get all RolePermission details by permissionId")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Retrieve all available RolePermissions and their details that match the given permissionId",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = RolePermissionVo.class))) }),
        @ApiResponse(responseCode = "400", description = "RolePermission permissionId is invalid",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
        @ApiResponse(responseCode = "404", description = "No RolePermissions available with the given permissionId",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("permissionid/{permissionId}")
    public List<RolePermissionVo> getAllRolePermissionsByPermission(@PathVariable String permissionId) throws RolePermissionException {
        log.debug("Requesting all available rolePermissions with given permissionId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(permissionId))) {
            try {
                Long actualPermissionId = Long.parseLong(permissionId);
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_VALID.getValue(), actualPermissionId);
                List<RolePermissionVo> matchedByPermissionId = service.retrieveAllMatchingDetailsByPermission(actualPermissionId);
                log.debug("Responding with successful retrieval of existing rolePermission details by permissionId");
                return matchedByPermissionId;
            } catch (NumberFormatException e) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_INVALID.getValue(), permissionId);
                throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "permissionId", permissionId });
            }
        }
        log.debug("rolePermission permissionId is empty");
        throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "permissionId", permissionId });
    }

    @Operation(summary = "Get all RolePermission details by roleId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available RolePermissions and their details that match the given roleId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = RolePermissionVo.class))) }),
            @ApiResponse(responseCode = "400", description = "RolePermission roleId is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No RolePermissions available with the given roleId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("roleid/{roleId}")
    public List<RolePermissionVo> getAllRolePermissionsByRole(@PathVariable String roleId) throws RolePermissionException {
        log.debug("Requesting all available rolePermissions with given roleId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(roleId))) {
            try {
                Long actualRoleId = Long.parseLong(roleId);
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_VALID.getValue(), actualRoleId);
                List<RolePermissionVo> matchedByPermissionId = service.retrieveAllMatchingDetailsByRole(actualRoleId);
                log.debug("Responding with successful retrieval of existing rolePermission details by roleId");
                return matchedByPermissionId;
            } catch (NumberFormatException e) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_INVALID.getValue(), roleId);
                throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "roleId", roleId });
            }
        }
        log.debug("rolePermission roleId is empty");
        throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "roleId", roleId });
    }

    @Operation(summary = "Get RolePermission details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of RolePermission that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RolePermissionVo.class)) }),
            @ApiResponse(responseCode = "400", description = "RolePermission id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No RolePermission found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public RolePermissionVo getRolePermissionDetailsById(@PathVariable String id) throws RolePermissionException {
        log.debug("Requesting all available rolePermissions by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_VALID.getValue(), id);
                RolePermissionVo rolePermissionDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing rolePermission details by id");
                return rolePermissionDetails;
            } catch (NumberFormatException e) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_INVALID.getValue(), id);
                throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_ID_EMPTY.getValue());
        throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
