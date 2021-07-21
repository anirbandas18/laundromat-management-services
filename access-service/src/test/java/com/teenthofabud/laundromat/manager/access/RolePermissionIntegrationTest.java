package com.teenthofabud.laundromat.manager.access;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationEntity;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionEntity;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceEntity;
import com.teenthofabud.laundromat.manager.access.role.data.RoleEntity;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionEntity;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionForm;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionVo;
import org.junit.Assert;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
@DirtiesContext(classMode = DirtiesContext.ClassMode.BEFORE_EACH_TEST_METHOD)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.ANY)
public class RolePermissionIntegrationTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final Long CREATED_BY_USER_ID = 1L;

    private static final String ROLE_PERMISSION_URI = "/rolepermission";
    private static final String ROLE_PERMISSION_URI_BY_ID = "/rolepermission/{id}";
    private static final String ROLE_PERMISSION_URI_BY_USER_TYPE_ID = "/rolepermission/permissionid/{permissionId}";
    private static final String ROLE_PERMISSION_URI_BY_ROLE_ID = "/rolepermission/roleid/{roleId}";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper om;

    @Autowired
    private EntityManager em;

    private ResourceEntity resourceEntity1;
    private ResourceEntity resourceEntity2;

    private OperationEntity operationEntity1;
    private OperationEntity operationEntity2;

    private PermissionEntity permissionEntity1;
    private PermissionEntity permissionEntity2;

    private RoleEntity roleEntity1;
    private RoleEntity roleEntity2;

    private RolePermissionForm rolePermissionForm;
    private RolePermissionVo rolePermissionVo1;
    private RolePermissionVo rolePermissionVo2;
    private RolePermissionVo rolePermissionVo3;
    private RolePermissionEntity rolePermissionEntity1;
    private RolePermissionEntity rolePermissionEntity2;
    private RolePermissionEntity rolePermissionEntity3;
    private List<PatchOperationForm> patches;

    @BeforeAll
    private void setUp() {
        rolePermissionForm = new RolePermissionForm();
        rolePermissionForm.setPermissionId(2L);
        rolePermissionForm.setRoleId(2L);

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/permissionId", "2"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/roleId", "2"));

        rolePermissionVo1 = new RolePermissionVo();
        rolePermissionVo1.setId(1L);
        rolePermissionVo1.setActive(Boolean.TRUE);
        rolePermissionVo1.setPermissionId(1L);
        rolePermissionVo1.setRoleId(1L);

        rolePermissionVo2 = new RolePermissionVo();
        rolePermissionVo2.setId(2L);
        rolePermissionVo2.setActive(Boolean.TRUE);
        rolePermissionVo2.setPermissionId(1L);
        rolePermissionVo2.setRoleId(2L);

        rolePermissionVo3 = new RolePermissionVo();
        rolePermissionVo3.setId(3L);
        rolePermissionVo3.setActive(Boolean.TRUE);
        rolePermissionVo3.setPermissionId(2L);
        rolePermissionVo3.setRoleId(1L);

        resourceEntity1 = new ResourceEntity();
        resourceEntity1.setActive(Boolean.TRUE);
        resourceEntity1.setCreatedBy(CREATED_BY_USER_ID);
        resourceEntity1.setCreatedOn(LocalDateTime.now());
        resourceEntity1.setModifiedBy(CREATED_BY_USER_ID);
        resourceEntity1.setModifiedOn(LocalDateTime.now());
        resourceEntity1.setVersion(0);
        resourceEntity1.setName("Resource 1");
        resourceEntity1.setDescription("Description of Resource 1");

        resourceEntity2 = new ResourceEntity();
        resourceEntity2.setActive(Boolean.TRUE);
        resourceEntity2.setCreatedBy(CREATED_BY_USER_ID);
        resourceEntity2.setCreatedOn(LocalDateTime.now());
        resourceEntity2.setModifiedBy(CREATED_BY_USER_ID);
        resourceEntity2.setModifiedOn(LocalDateTime.now());
        resourceEntity2.setVersion(0);
        resourceEntity2.setName("Resource 2");
        resourceEntity2.setDescription("Description of Resource 2");

        operationEntity1 = new OperationEntity();
        operationEntity1.setActive(Boolean.TRUE);
        operationEntity1.setCreatedBy(CREATED_BY_USER_ID);
        operationEntity1.setCreatedOn(LocalDateTime.now());
        operationEntity1.setModifiedBy(CREATED_BY_USER_ID);
        operationEntity1.setModifiedOn(LocalDateTime.now());
        operationEntity1.setVersion(0);
        operationEntity1.setName("Operation 1");
        operationEntity1.setDescription("Description of Operation 1");

        operationEntity2 = new OperationEntity();
        operationEntity2.setActive(Boolean.TRUE);
        operationEntity2.setCreatedBy(CREATED_BY_USER_ID);
        operationEntity2.setCreatedOn(LocalDateTime.now());
        operationEntity2.setModifiedBy(CREATED_BY_USER_ID);
        operationEntity2.setModifiedOn(LocalDateTime.now());
        operationEntity2.setVersion(0);
        operationEntity2.setName("Operation 2");
        operationEntity2.setDescription("Description of Operation 2");

        permissionEntity1 = new PermissionEntity();
        permissionEntity1.setActive(Boolean.TRUE);
        permissionEntity1.setCreatedBy(CREATED_BY_USER_ID);
        permissionEntity1.setCreatedOn(LocalDateTime.now());
        permissionEntity1.setModifiedBy(CREATED_BY_USER_ID);
        permissionEntity1.setModifiedOn(LocalDateTime.now());
        permissionEntity1.setVersion(0);
        /*permissionEntity1.setName("Permission 1");
        permissionEntity1.setDescription("Description of Permission 1");*/

        permissionEntity2 = new PermissionEntity();
        permissionEntity2.setActive(Boolean.TRUE);
        permissionEntity2.setCreatedBy(CREATED_BY_USER_ID);
        permissionEntity2.setCreatedOn(LocalDateTime.now());
        permissionEntity2.setModifiedBy(CREATED_BY_USER_ID);
        permissionEntity2.setModifiedOn(LocalDateTime.now());
        permissionEntity2.setVersion(0);
        /*permissionEntity2.setName("Permission 2");
        permissionEntity2.setDescription("Description of Permission 2");*/

        roleEntity1 = new RoleEntity();
        roleEntity1.setActive(Boolean.TRUE);
        roleEntity1.setCreatedBy(CREATED_BY_USER_ID);
        roleEntity1.setCreatedOn(LocalDateTime.now());
        roleEntity1.setModifiedBy(CREATED_BY_USER_ID);
        roleEntity1.setModifiedOn(LocalDateTime.now());
        roleEntity1.setVersion(0);
        roleEntity1.setName("Role 1");
        roleEntity1.setDescription("Description of Role 1");

        roleEntity2 = new RoleEntity();
        roleEntity2.setActive(Boolean.TRUE);
        roleEntity2.setCreatedBy(CREATED_BY_USER_ID);
        roleEntity2.setCreatedOn(LocalDateTime.now());
        roleEntity2.setModifiedBy(CREATED_BY_USER_ID);
        roleEntity2.setModifiedOn(LocalDateTime.now());
        roleEntity2.setVersion(0);
        roleEntity2.setName("Role 2");
        roleEntity2.setDescription("Description of Role 2");

        rolePermissionEntity1 = new RolePermissionEntity();
        rolePermissionEntity1.setActive(Boolean.TRUE);
        rolePermissionEntity1.setCreatedBy(CREATED_BY_USER_ID);
        rolePermissionEntity1.setCreatedOn(LocalDateTime.now());
        rolePermissionEntity1.setModifiedBy(CREATED_BY_USER_ID);
        rolePermissionEntity1.setModifiedOn(LocalDateTime.now());
        rolePermissionEntity1.setVersion(0);

        rolePermissionEntity2 = new RolePermissionEntity();
        rolePermissionEntity2.setActive(Boolean.TRUE);
        rolePermissionEntity2.setCreatedBy(CREATED_BY_USER_ID);
        rolePermissionEntity2.setCreatedOn(LocalDateTime.now());
        rolePermissionEntity2.setModifiedBy(CREATED_BY_USER_ID);
        rolePermissionEntity2.setModifiedOn(LocalDateTime.now());
        rolePermissionEntity2.setVersion(0);

        rolePermissionEntity3 = new RolePermissionEntity();
        rolePermissionEntity3.setActive(Boolean.FALSE);
        rolePermissionEntity3.setCreatedBy(CREATED_BY_USER_ID);
        rolePermissionEntity3.setCreatedOn(LocalDateTime.now());
        rolePermissionEntity3.setModifiedBy(CREATED_BY_USER_ID);
        rolePermissionEntity3.setModifiedOn(LocalDateTime.now());
        rolePermissionEntity3.setVersion(0);

        om.registerModule(new Jdk8Module());
        om.registerModule(new JavaTimeModule());

    }

    @BeforeEach
    private void init() {
        resourceEntity1 = em.merge(resourceEntity1);
        resourceEntity2 = em.merge(resourceEntity2);
        operationEntity1 = em.merge(operationEntity1);
        operationEntity2 = em.merge(operationEntity2);

        permissionEntity1.setResource(resourceEntity1);
        permissionEntity1.setOperation(operationEntity1);
        permissionEntity2.setResource(resourceEntity2);
        permissionEntity2.setOperation(operationEntity2);

        permissionEntity1 = em.merge(permissionEntity1);
        permissionEntity2 = em.merge(permissionEntity2);

        roleEntity1 = em.merge(roleEntity1);
        roleEntity2 = em.merge(roleEntity2);

        rolePermissionEntity1.setPermission(permissionEntity1);
        rolePermissionEntity1.setRole(roleEntity1);
        rolePermissionEntity2.setPermission(permissionEntity1);
        rolePermissionEntity2.setRole(roleEntity2);
        rolePermissionEntity3.setPermission(permissionEntity2);
        rolePermissionEntity3.setRole(roleEntity1);

        em.merge(rolePermissionEntity1);
        em.merge(rolePermissionEntity2);
        em.merge(rolePermissionEntity3);
    }

    @AfterEach
    private void destroy() {
        rolePermissionEntity1.setPermission(null);
        rolePermissionEntity1.setRole(null);
        rolePermissionEntity2.setPermission(null);
        rolePermissionEntity2.setRole(null);
        rolePermissionEntity3.setPermission(null);
        rolePermissionEntity3.setRole(null);

        em.remove(rolePermissionEntity1);
        em.remove(rolePermissionEntity2);
        em.remove(rolePermissionEntity3);

        permissionEntity1.setResource(null);
        permissionEntity1.setOperation(null);
        permissionEntity2.setResource(null);
        permissionEntity2.setOperation(null);

        em.remove(permissionEntity1);
        em.remove(permissionEntity2);
        em.remove(roleEntity1);
        em.remove(roleEntity2);

        em.remove(resourceEntity1);
        em.remove(resourceEntity2);
        em.remove(operationEntity1);
        em.remove(operationEntity2);

        rolePermissionForm = new RolePermissionForm();
        rolePermissionForm.setPermissionId(2L);
        rolePermissionForm.setRoleId(2L);

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/permissionId", "2"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/roleId", "2"));
    }

    @Test
    public void test_RolePermission_Post_ShouldReturn_201Response_And_NewRolePermissionId_WhenPosted_WithValidRolePermissionForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(ROLE_PERMISSION_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(rolePermissionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assert.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_RolePermission_Post_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_WithEmptyPermissionId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "permissionId";
        rolePermissionForm.setPermissionId(null);

        mvcResult = mockMvc.perform(post(ROLE_PERMISSION_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(rolePermissionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_RolePermission_Post_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_WithEmptyRoleId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "roleId";
        rolePermissionForm.setRoleId(null);

        mvcResult = mockMvc.perform(post(ROLE_PERMISSION_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(rolePermissionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_RolePermission_Post_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_004_WhenRequested_WithDuplicateRolePermission() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String field1Name = "permissionId";
        String field2Name = "roleId";
        rolePermissionForm.setPermissionId(permissionEntity1.getId());
        rolePermissionForm.setRoleId(roleEntity1.getId());

        mvcResult = mockMvc.perform(post(ROLE_PERMISSION_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(rolePermissionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Name));
    }

    @Test
    public void test_RolePermission_Post_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenPosted_WithNoRolePermissionForm() throws Exception {
        long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(ROLE_PERMISSION_URI)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_RolePermission_Get_ShouldReturn_200Response_And_RolePermissionListNaturallyOrdered_WhenRequested_ForAllRolePermissions() throws Exception {
        MvcResult mvcResult = null;
        Set<RolePermissionVo> studentList = new TreeSet<>(Arrays.asList(rolePermissionVo1, rolePermissionVo2, rolePermissionVo3));
        long expectedRolePermissionVoCount = 3;

        mvcResult = this.mockMvc.perform(get(ROLE_PERMISSION_URI))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(expectedRolePermissionVoCount, om.readValue(mvcResult.getResponse().getContentAsString(), RolePermissionVo[].class).length);
    }

    @Test
    public void test_RolePermission_Get_ShouldReturn_200Response_And_RolePermissionDetails_WhenRequested_ById() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ROLE_PERMISSION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(om.writeValueAsString(rolePermissionVo1), mvcResult.getResponse().getContentAsString());
        Assert.assertEquals(rolePermissionVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), RolePermissionVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_RolePermission_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ROLE_PERMISSION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_RolePermission_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_002_WhenRequested_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ROLE_PERMISSION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_RolePermission_Get_ShouldReturn_200Response_And_MatchingRolePermissionDetails_WhenRequested_ByPermissionId() throws Exception {
        String permissionId = "1";
        List<RolePermissionVo> students = Arrays.asList(rolePermissionVo1, rolePermissionVo2);
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ROLE_PERMISSION_URI_BY_USER_TYPE_ID, permissionId))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(2, om.readValue(mvcResult.getResponse().getContentAsString(), RolePermissionVo[].class).length);
    }

    @Test
    public void test_RolePermission_Get_ShouldReturn_200Response_And_MatchingRolePermissionDetails_WhenRequested_ByRoleId() throws Exception {
        String roleId = "2";
        List<RolePermissionVo> students = Arrays.asList(rolePermissionVo1, rolePermissionVo3);
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ROLE_PERMISSION_URI_BY_ROLE_ID, roleId))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(1, om.readValue(mvcResult.getResponse().getContentAsString(), RolePermissionVo[].class).length);
    }

    @Test
    public void test_RolePermission_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ByEmptyPermissionId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "permissionId";

        mvcResult = this.mockMvc.perform(get(ROLE_PERMISSION_URI_BY_USER_TYPE_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_RolePermission_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ByEmptyRoleId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "roleId";

        mvcResult = this.mockMvc.perform(get(ROLE_PERMISSION_URI_BY_ROLE_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_RolePermission_Get_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenRequested_ByAbsentPermissionId() throws Exception {
        MvcResult mvcResult = null;
        String permissionId = "22";
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "permissionId";

        mvcResult = this.mockMvc.perform(get(ROLE_PERMISSION_URI_BY_USER_TYPE_ID, permissionId))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(permissionId));
    }

    @Test
    public void test_RolePermission_Get_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenRequested_ByAbsentRoleId() throws Exception {
        MvcResult mvcResult = null;
        String roleId = "22";
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "roleId";

        mvcResult = this.mockMvc.perform(get(ROLE_PERMISSION_URI_BY_ROLE_ID, roleId))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(roleId));
    }

    @Test
    public void test_RolePermission_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        Long id = 2l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(ROLE_PERMISSION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_RolePermission_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001__WhenDeleted_ByEmptyId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ROLE_PERMISSION_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_RolePermission_Delete_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenDeleted_ByInvalidId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ROLE_PERMISSION_URI_BY_ID, "r"))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_RolePermission_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_005_WhenDeleted_ByInactiveId() throws Exception {
        Long id = 3l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(ROLE_PERMISSION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_RolePermission_Delete_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenDeleted_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ROLE_PERMISSION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_RolePermission_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndRolePermissionDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        rolePermissionForm.setPermissionId(2L);
        rolePermissionForm.setPermissionId(2L);

        mvcResult = this.mockMvc.perform(put(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(rolePermissionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_RolePermission_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenUpdatedBy_EmptyInvalidId_AndRolePermissionDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(rolePermissionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_RolePermission_Put_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ByAbsentId_AndRolePermissionDetails() throws Exception {
        Long id = 41l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(rolePermissionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_RolePermission_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_005_WhenUpdated_ByInactiveId_AndRolePermissionDetails() throws Exception {
        Long id = 3l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(rolePermissionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_RolePermission_Put_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndNoRolePermissionDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @ParameterizedTest
    @ValueSource(longs = { 0L, -1L })
    public void test_RolePermission_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidPermissionId(Long permissionId) throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "permissionId";
        rolePermissionForm.setPermissionId(permissionId);

        mvcResult = mockMvc.perform(put(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(rolePermissionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(longs = { 0L, -1L })
    public void test_RolePermission_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidRoleId(Long roleId) throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "roleId";
        rolePermissionForm.setRoleId(roleId);

        mvcResult = mockMvc.perform(put(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(rolePermissionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_RolePermission_Put_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndEmptyRolePermissionDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new RolePermissionForm())))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_RolePermission_Put_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_004_WhenUpdated_ById_AndDuplicateRolePermissionDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String field1Name = "roleId";
        rolePermissionForm.setRoleId(1L);

        mvcResult = mockMvc.perform(put(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(rolePermissionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_RolePermission_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndRolePermissionDetails() throws Exception {
        Long id = 3l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_RolePermission_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndRolePermissionDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ROLE_PERMISSION_URI_BY_ID, " ")
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_RolePermission_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ByInvalidId_AndRolePermissionDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id));
    }

    @Test
    public void test_RolePermission_Patch_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ByAbsentId_AndRolePermissionDetails() throws Exception {
        Long id = 411l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_RolePermission_Patch_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ById_AndDuplicateRolePermissionDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String fieldName = "roleId";
        String fieldValue = "2";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, fieldValue));


        mvcResult = this.mockMvc.perform(patch(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldValue));
    }

    @Test
    public void test_RolePermission_Patch_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndNoRolePermissionDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(ROLE_PERMISSION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_RolePermission_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_RolePermission_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_RolePermission_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidDefinitionOfRolePermissionAttribute() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(ROLE_PERMISSION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

}