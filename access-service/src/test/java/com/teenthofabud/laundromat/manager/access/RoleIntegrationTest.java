package com.teenthofabud.laundromat.manager.access;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.role.data.RoleEntity;
import com.teenthofabud.laundromat.manager.access.role.data.RoleForm;
import com.teenthofabud.laundromat.manager.access.role.data.RoleVo;
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
public class RoleIntegrationTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final Long CREATED_BY_USER_ID = 1L;

    private static final String ROLE_URI = "/role";
    private static final String ROLE_URI_BY_ID = "/role/{id}";
    private static final String ROLE_URI_BY_NAME = "/role/name/{name}";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper om;

    @Autowired
    private EntityManager em;

    private RoleForm roleForm;
    private RoleVo roleVo1;
    private RoleVo roleVo2;
    private RoleVo roleVo3;
    private RoleVo roleVo4;
    private RoleEntity roleEntity1;
    private RoleEntity roleEntity2;
    private RoleEntity roleEntity3;
    private RoleEntity roleEntity4;
    private List<PatchOperationForm> patches;

    @BeforeAll
    private void setUp() {
        roleForm = new RoleForm();
        roleForm.setName("Demo Role");
        roleForm.setDescription("This is for e2e testing of services");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample Role"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this Role role"));

        roleVo1 = new RoleVo();
        roleVo1.setId(1L);
        roleVo1.setActive(Boolean.TRUE);
        roleVo1.setName("Test Role 1");
        roleVo1.setDescription("This belongs to group 1 for e2e testing");

        roleVo2 = new RoleVo();
        roleVo2.setId(2L);
        roleVo2.setActive(Boolean.TRUE);
        roleVo2.setName("Test Role 2");
        roleVo2.setDescription("This belongs to group 1 for e2e testing");

        roleVo3 = new RoleVo();
        roleVo3.setId(3L);
        roleVo3.setActive(Boolean.TRUE);
        roleVo3.setName("Sample Role 3");
        roleVo3.setDescription("This belongs to group 2 for e2e testing");

        roleVo4 = new RoleVo();
        roleVo4.setId(4L);
        roleVo4.setActive(Boolean.FALSE);
        roleVo4.setName("Sample Role 4");
        roleVo4.setDescription("This belongs to group 2 for e2e testing");

        roleEntity1 = new RoleEntity();
        roleEntity1.setActive(Boolean.TRUE);
        roleEntity1.setCreatedBy(CREATED_BY_USER_ID);
        roleEntity1.setCreatedOn(LocalDateTime.now());
        roleEntity1.setModifiedBy(CREATED_BY_USER_ID);
        roleEntity1.setModifiedOn(LocalDateTime.now());
        roleEntity1.setVersion(0);
        roleEntity1.setName("Test Role 1");
        roleEntity1.setDescription("This belongs to group 1 for e2e testing");

        roleEntity2 = new RoleEntity();
        roleEntity2.setActive(Boolean.TRUE);
        roleEntity2.setCreatedBy(CREATED_BY_USER_ID);
        roleEntity2.setCreatedOn(LocalDateTime.now());
        roleEntity2.setModifiedBy(CREATED_BY_USER_ID);
        roleEntity2.setModifiedOn(LocalDateTime.now());
        roleEntity2.setVersion(0);
        roleEntity2.setName("Test Role 2");
        roleEntity2.setDescription("This belongs to group 1 for e2e testing");


        roleEntity3 = new RoleEntity();
        roleEntity3.setActive(Boolean.TRUE);
        roleEntity3.setCreatedBy(CREATED_BY_USER_ID);
        roleEntity3.setCreatedOn(LocalDateTime.now());
        roleEntity3.setModifiedBy(CREATED_BY_USER_ID);
        roleEntity3.setModifiedOn(LocalDateTime.now());
        roleEntity3.setVersion(0);
        roleEntity3.setName("Sample Role 3");
        roleEntity3.setDescription("This belongs to group 2 for e2e testing");


        roleEntity4 = new RoleEntity();
        roleEntity4.setActive(Boolean.FALSE);
        roleEntity4.setCreatedBy(CREATED_BY_USER_ID);
        roleEntity4.setCreatedOn(LocalDateTime.now());
        roleEntity4.setModifiedBy(CREATED_BY_USER_ID);
        roleEntity4.setModifiedOn(LocalDateTime.now());
        roleEntity4.setVersion(0);
        roleEntity4.setName("Sample Role 4");
        roleEntity4.setDescription("This belongs to group 2 for e2e testing");

        om.registerModule(new Jdk8Module());
        om.registerModule(new JavaTimeModule());

    }

    @BeforeEach
    private void init() {
        em.merge(roleEntity1);
        em.merge(roleEntity2);
        em.merge(roleEntity3);
        em.merge(roleEntity4);
    }

    @AfterEach
    private void destroy() {
        em.remove(roleEntity1);
        em.remove(roleEntity2);
        em.remove(roleEntity3);
        em.remove(roleEntity4);

        roleForm = new RoleForm();
        roleForm.setName("Demo Role");
        roleForm.setDescription("This is for e2e testing of services");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample Role"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this Role role"));
    }

    @Test
    public void test_Role_Post_ShouldReturn_201Response_And_NewRoleId_WhenPosted_WithValidRoleForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(ROLE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(roleForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assert.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Role_Post_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        roleForm.setName("");

        mvcResult = mockMvc.perform(post(ROLE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(roleForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Role_Post_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_004_WhenRequested_WithDuplicateRole() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String field1Name = "name";
        roleForm.setName("Test Role 1");

        mvcResult = mockMvc.perform(post(ROLE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(roleForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_Role_Post_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenPosted_WithNoRoleForm() throws Exception {
        long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(ROLE_URI)
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
    public void test_Role_Get_ShouldReturn_200Response_And_RoleListNaturallyOrdered_WhenRequested_ForAllRoles() throws Exception {
        MvcResult mvcResult = null;
        Set<RoleVo> studentList = new TreeSet<>(Arrays.asList(roleVo1, roleVo2, roleVo3, roleVo4));
        long expectedRoleVoCount = 4;

        mvcResult = this.mockMvc.perform(get(ROLE_URI))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(expectedRoleVoCount, om.readValue(mvcResult.getResponse().getContentAsString(), RoleVo[].class).length);
    }

    @Test
    public void test_Role_Get_ShouldReturn_200Response_And_RoleDetails_WhenRequested_ById() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ROLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(om.writeValueAsString(roleVo1), mvcResult.getResponse().getContentAsString());
        Assert.assertEquals(roleVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), RoleVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Role_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ROLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Role_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_002_WhenRequested_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ROLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Role_Get_ShouldReturn_200Response_And_MatchingRoleDetails_WhenRequested_ByName() throws Exception {
        String name = "Test";
        List<RoleVo> students = Arrays.asList(roleVo1, roleVo2);
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ROLE_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(2, om.readValue(mvcResult.getResponse().getContentAsString(), RoleVo[].class).length);
    }

    @Test
    public void test_Role_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ByEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(ROLE_URI_BY_NAME, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Role_Get_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenRequested_ByAbsentName() throws Exception {
        MvcResult mvcResult = null;
        String name = "kk";
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(ROLE_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(name));
    }

    @Test
    public void test_Role_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        Long id = 3l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(ROLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Role_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001__WhenDeleted_ByEmptyId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ROLE_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Role_Delete_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenDeleted_ByInvalidId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ROLE_URI_BY_ID, "r"))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Role_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_005_WhenDeleted_ByInactiveId() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(ROLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Role_Delete_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenDeleted_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ROLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Role_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndRoleDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        roleForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(ROLE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(roleForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Role_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenUpdatedBy_EmptyInvalidId_AndRoleDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ROLE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(roleForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Role_Put_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ByAbsentId_AndRoleDetails() throws Exception {
        Long id = 41l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ROLE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(roleForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Role_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_005_WhenUpdated_ByInactiveId_AndRoleDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(ROLE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(roleForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Role_Put_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndNoRoleDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(ROLE_URI_BY_ID, id)
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
    public void test_Role_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        roleForm.setName("");

        mvcResult = mockMvc.perform(put(ROLE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(roleForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Role_Put_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndEmptyRoleDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(ROLE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new RoleForm())))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Role_Put_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_004_WhenUpdated_ById_AndDuplicateRoleDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String field1Name = "name";
        roleForm.setName(roleEntity1.getName());

        mvcResult = mockMvc.perform(put(ROLE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(roleForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_Role_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndRoleDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(ROLE_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Role_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndRoleDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ROLE_URI_BY_ID, " ")
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
    public void test_Role_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ByInvalidId_AndRoleDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ROLE_URI_BY_ID, id)
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
    public void test_Role_Patch_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ByAbsentId_AndRoleDetails() throws Exception {
        Long id = 411l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ROLE_URI_BY_ID, id)
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
    public void test_Role_Patch_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ById_AndDuplicateRoleDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String fieldName = "name";
        String fieldValue = "Sample Role 3";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", fieldValue));


        mvcResult = this.mockMvc.perform(patch(ROLE_URI_BY_ID, id)
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
    public void test_Role_Patch_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndNoRoleDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(ROLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Role_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(ROLE_URI_BY_ID, id)
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
    public void test_Role_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(ROLE_URI_BY_ID, id)
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
    public void test_Role_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidDefinitionOfRoleAttribute() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(ROLE_URI_BY_ID, id)
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