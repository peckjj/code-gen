
// p4-codegen.c

/**
 * @file p4-codegen.c
 * @brief Compiler phase 4: code generation
 */
#include "p4-codegen.h"

/**
 * @brief State/data for the code generator visitor
 */
typedef struct CodeGenData
{
    /**
     * @brief Reference to the epilogue jump label for the current function
     */
    Operand current_epilogue_jump_label;
    Operand brk_label; // Track label for jump after break statement
    Operand cont_label; // Track label for jump after continue statements
    Operand return_reg; // Track return reg for functions with mulitple return statements
    bool saw_return; // True if a return statement has already been seen in the function declaration
    /* add any new desired state information (and clean it up in CodeGenData_free) */
} CodeGenData;

/**
 * @brief Allocate memory for code gen data
 * 
 * @returns Pointer to allocated structure
 */
CodeGenData* CodeGenData_new ()
{
    CodeGenData* data = (CodeGenData*)calloc(1, sizeof(CodeGenData));
    CHECK_MALLOC_PTR(data);
    data->current_epilogue_jump_label = empty_operand();
    data->brk_label = empty_operand();
    data->cont_label = empty_operand();
    data->return_reg = empty_operand();
    data->saw_return = false;
    return data;
}

/**
 * @brief Deallocate memory for code gen data
 * 
 * @param data Pointer to the structure to be deallocated
 */
void CodeGenData_free (CodeGenData* data)
{
    /* free everything in data that is allocated on the heap */

    /* free "data" itself */
    free(data);
}

/**
 * @brief Macro for more convenient access to the error list inside a @c visitor
 * data structure
 */
#define DATA ((CodeGenData*)visitor->data)

/**
 * @brief Fills a register with the base address of a variable.
 * 
 * @param node AST node to emit code into (if needed)
 * @param variable Desired variable
 * @returns Virtual register that contains the base address
 */
Operand var_base (ASTNode* node, Symbol* variable)
{
    Operand reg = empty_operand();
    switch (variable->location) {
        case STATIC_VAR:
            reg = virtual_register();
            ASTNode_emit_insn(node,
                    ILOCInsn_new_2op(LOAD_I, int_const(variable->offset), reg));
            break;
        case STACK_PARAM:
        case STACK_LOCAL:
            reg = base_register();
            break;
        default:
            break;
    }
    return reg;
}

/**
 * @brief Calculates the offset of a scalar variable reference and fills a register with that offset.
 * 
 * @param node AST node to emit code into (if needed)
 * @param variable Desired variable
 * @returns Virtual register that contains the base address
 */
Operand var_offset (ASTNode* node, Symbol* variable)
{
    Operand op = empty_operand();
    switch (variable->location) {
        case STATIC_VAR:    op = int_const(0); break;
        case STACK_PARAM:
        case STACK_LOCAL:   op = int_const(variable->offset);
        default:
            break;
    }
    return op;
}

#ifndef SKIP_IN_DOXYGEN

/*
 * Macros for more convenient instruction generation
 */

#define EMIT0OP(FORM)             ASTNode_emit_insn(node, ILOCInsn_new_0op(FORM))
#define EMIT1OP(FORM,OP1)         ASTNode_emit_insn(node, ILOCInsn_new_1op(FORM,OP1))
#define EMIT2OP(FORM,OP1,OP2)     ASTNode_emit_insn(node, ILOCInsn_new_2op(FORM,OP1,OP2))
#define EMIT3OP(FORM,OP1,OP2,OP3) ASTNode_emit_insn(node, ILOCInsn_new_3op(FORM,OP1,OP2,OP3))

void CodeGenVisitor_gen_program (NodeVisitor* visitor, ASTNode* node)
{
    /*
     * make sure "code" attribute exists at the program level even if there are
     * no functions (although this shouldn't happen if static analysis is run
     * first); also, don't include a print function here because there's not
     * really any need to re-print all the functions in the program node *
     */
    ASTNode_set_attribute(node, "code", InsnList_new(), (Destructor)InsnList_free);

    /* copy code from each function */
    FOR_EACH(ASTNode*, func, node->program.functions) {
        ASTNode_copy_code(node, func);
    }
}

void CodeGenVisitor_previsit_funcdecl (NodeVisitor* visitor, ASTNode* node)
{
    /* generate a label reference for the epilogue that can be used while
     * generating the rest of the function (e.g., to be used when generating
     * code for a "return" statement) */
    DATA->current_epilogue_jump_label = anonymous_label();
}

void CodeGenVisitor_gen_funcdecl (NodeVisitor* visitor, ASTNode* node)
{
    long adjust_stack = ASTNode_get_int_attribute(node, "localSize");

    /* every function begins with the corresponding call label */
    EMIT1OP(LABEL, call_label(node->funcdecl.name));

    /* BOILERPLATE: TODO: implement prologue */
    EMIT1OP(PUSH, base_register());                                   // push BP
    EMIT2OP(I2I, stack_register(), base_register());                  // i2i SP => BP
    EMIT3OP(ADD_I, stack_register(), int_const(-adjust_stack), stack_register()); // addI SP, 0 => SP

    /* copy code from body */
    ASTNode_copy_code(node, node->funcdecl.body);

    if (node->funcdecl.return_type != VOID)
    {
        EMIT2OP(I2I, DATA->return_reg, return_register()); // i2i <ret-REG> => ret
    } 

    EMIT1OP(JUMP, DATA->current_epilogue_jump_label);

    EMIT1OP(LABEL, DATA->current_epilogue_jump_label);
    /* BOILERPLATE: TODO: implement epilogue */
    EMIT2OP(I2I, base_register(), stack_register());                                // i2i BP => SP
    EMIT1OP(POP, base_register());                                                  // pop BP
    EMIT0OP(RETURN);

    DATA->saw_return = false;
}

#endif

void vardecl_pre(NodeVisitor* visitor, ASTNode* node)
{
    return;
}

void vardecl_post(NodeVisitor* visitor, ASTNode* node)
{
    return;
}

void block_pre(NodeVisitor* visitor, ASTNode* node)
{
    return;
}

void block_post(NodeVisitor* visitor, ASTNode* node)
{
    FOR_EACH(ASTNode*, statement, node->block.statements)
    {
        ASTNode_copy_code(node, statement);
        if (statement->type == RETURNSTMT)
        {
            ASTNode_set_temp_reg(node, ASTNode_get_temp_reg(statement));
        }
    }

    return;
}

void assignment_pre(NodeVisitor* visitor, ASTNode* node)
{
    return;
}

void assignment_post(NodeVisitor* visitor, ASTNode* node)
{
    if (node->assignment.location->location.index)
    {
        Operand r0 = virtual_register();

        int size = 8;
        if (lookup_symbol(node->assignment.location, node->assignment.location->location.name)->type == BOOL)
        {
            size = 1;
        }

        ASTNode_copy_code(node, node->assignment.value);
        ASTNode_copy_code(node, node->assignment.location->location.index);
        EMIT3OP(MULT_I, ASTNode_get_temp_reg(node->assignment.location->location.index), int_const(size), r0);
        EMIT3OP(STORE_AO, ASTNode_get_temp_reg(node->assignment.value), var_base(node, lookup_symbol(node->assignment.location, node->assignment.location->location.name)), r0);
    } else
    {
        ASTNode_copy_code(node, node->assignment.value);
        EMIT3OP(STORE_AI, ASTNode_get_temp_reg(node->assignment.value), var_base(node, lookup_symbol(node->assignment.location, node->assignment.location->location.name)), var_offset(node, lookup_symbol(node->assignment.location, node->assignment.location->location.name)));
    }

    return;
}

void literal_pre(NodeVisitor* visitor, ASTNode* node)
{
    return;
}

void literal_post(NodeVisitor* visitor, ASTNode* node)
{
    Operand store = virtual_register();

    if (node->literal.type == INT)
    {
        EMIT2OP(LOAD_I, int_const(node->literal.integer), store);
    } else {
        if (node->literal.boolean)
        {
            EMIT2OP(LOAD_I, int_const(1), store);
        } else
        {
            EMIT2OP(LOAD_I, int_const(0), store);
        }
    }

    ASTNode_set_temp_reg(node, store);

    return;
}

void return_pre(NodeVisitor* visitor, ASTNode* node)
{
    return;
}

void return_post(NodeVisitor* visitor, ASTNode* node)
{
    ASTNode_copy_code(node, node->funcreturn.value);
    ASTNode_set_temp_reg(node, ASTNode_get_temp_reg(node->funcreturn.value));

    if (DATA->saw_return)
    {
        EMIT2OP(I2I, ASTNode_get_temp_reg(node), DATA->return_reg);
        ASTNode_set_temp_reg(node, DATA->return_reg);
        return;
    }

    DATA->saw_return = true;
    DATA->return_reg = ASTNode_get_temp_reg(node);

    return;
}

void location_pre(NodeVisitor* visitor, ASTNode* node)
{
    return;
}

void location_post(NodeVisitor* visitor, ASTNode* node)
{
    Operand store = virtual_register();
    if (node->location.index)
    {
        Operand r0 = virtual_register();

        int size = 8;
        if (lookup_symbol(node, node->location.name)->type == BOOL)
        {
            size = 1;
        }

        ASTNode_copy_code(node, node->location.index);
        EMIT3OP(MULT_I, ASTNode_get_temp_reg(node->location.index), int_const(size), r0);
        EMIT3OP(LOAD_AO, var_base(node, lookup_symbol(node, node->location.name)), r0, store);
    } else
    {
        EMIT3OP(LOAD_AI, var_base(node, lookup_symbol(node, node->location.name)), var_offset(node, lookup_symbol(node, node->location.name)), store);
    }
    ASTNode_set_temp_reg(node, store);
    return;
}

void binop_pre(NodeVisitor* visitor, ASTNode* node)
{
    return;
}

void binop_post(NodeVisitor* visitor, ASTNode* node)
{
    Operand store = virtual_register();

    ASTNode_copy_code(node, node->binaryop.left);
    ASTNode_copy_code(node, node->binaryop.right);

    InsnForm op;

    switch (node->binaryop.operator)
    {
        case ADDOP:
            op = ADD;
            break;
        case SUBOP:
            op = SUB;
            break;
        case MULOP:
            op = MULT;
            break;
        case DIVOP:
            op = DIV;
            break;
        case LTOP:
            op = CMP_LT;
            break;
        case LEOP:
            op = CMP_LE;
            break;
        case EQOP:
            op = CMP_EQ;
            break;
        case GTOP:
            op = CMP_GT;
            break;
        case GEOP:
            op = CMP_GE;
            break;
        case NEQOP:
            op = CMP_NE;
            break;
        default:
            break;
    }
    EMIT3OP(op, ASTNode_get_temp_reg(node->binaryop.left), ASTNode_get_temp_reg(node->binaryop.right), store);
    ASTNode_set_temp_reg(node, store);

    return;
}

void unop_pre(NodeVisitor* visitor, ASTNode* node)
{
    return;
}

void unop_post(NodeVisitor* visitor, ASTNode* node)
{
    Operand store = virtual_register();

    ASTNode_copy_code(node, node->unaryop.child);

    InsnForm op;

    switch (node->unaryop.operator)
    {
    case NEGOP:
        op = NEG;
        break;
    case NOTOP:
        op = NOT;
    default:
        break;
    }

    EMIT2OP(op, ASTNode_get_temp_reg(node->unaryop.child), store);
    ASTNode_set_temp_reg(node, store);

    return;
}

void conditional_pre(NodeVisitor* visitor, ASTNode* node)
{
    return;
}

void conditional_post(NodeVisitor* visitor, ASTNode* node)
{

    if (node->conditional.else_block != NULL)
    {
        ASTNode_copy_code(node, node->conditional.condition);
        Operand cond_reg = ASTNode_get_temp_reg(node->conditional.condition);
        Operand l1 = anonymous_label();
        Operand l2 = anonymous_label();
        Operand l3 = anonymous_label();
        EMIT3OP(CBR, cond_reg, l1, l2);
        EMIT1OP(LABEL, l1);
        ASTNode_copy_code(node, node->conditional.if_block);
        EMIT1OP(JUMP, l3);
        EMIT1OP(LABEL, l2);
        ASTNode_copy_code(node, node->conditional.else_block);
        EMIT1OP(LABEL, l3);
        return;
    }
    ASTNode_copy_code(node, node->conditional.condition);
    Operand cond_reg = ASTNode_get_temp_reg(node->conditional.condition);
    Operand l1 = anonymous_label();
    Operand l2 = anonymous_label();

    EMIT3OP(CBR, cond_reg, l1, l2);
    EMIT1OP(LABEL, l1);
    ASTNode_copy_code(node, node->conditional.if_block);
    EMIT1OP(LABEL, l2);

    ASTNode_set_temp_reg(node, cond_reg);

    return;
}

void while_pre(NodeVisitor* visitor, ASTNode* node)
{
    DATA->cont_label = anonymous_label();
    DATA->brk_label = anonymous_label();

    ASTNode_set_attribute(node, "l1", &DATA->cont_label, dummy_free);
    ASTNode_set_attribute(node, "l3", &DATA->brk_label, dummy_free);

    return;
}

void while_post(NodeVisitor* visitor, ASTNode* node)
{
    Operand cond_reg = ASTNode_get_temp_reg(node->whileloop.condition);
    Operand l2 = anonymous_label();
    Operand *l1 = (Operand*) ASTNode_get_attribute(node, "l1");
    Operand *l3 = (Operand*) ASTNode_get_attribute(node, "l3");

    EMIT1OP(LABEL, *l1);
    ASTNode_copy_code(node, node->whileloop.condition);
    EMIT3OP(CBR, cond_reg, l2, *l3);
    EMIT1OP(LABEL, l2);
    ASTNode_copy_code(node, node->whileloop.body);
    EMIT1OP(JUMP, *l1);
    EMIT1OP(LABEL, *l3);

    ASTNode_set_temp_reg(node, cond_reg);

    return;
}

void funccall_pre(NodeVisitor* visitor, ASTNode* node)
{
    return;
}

void funccall_post(NodeVisitor* visitor, ASTNode* node)
{
    if (strcmp(node->funccall.name, "print_int") == 0)
    {
        ASTNode_copy_code(node, node->funccall.arguments->head);
        EMIT1OP(PRINT, ASTNode_get_temp_reg(node->funccall.arguments->head));
        ASTNode_set_temp_reg(node, ASTNode_get_temp_reg(node->funccall.arguments->head));
        return;
    }

    if (strcmp(node->funccall.name, "print_bool") == 0)
    {
        ASTNode_copy_code(node, node->funccall.arguments->head);
        EMIT1OP(PRINT, ASTNode_get_temp_reg(node->funccall.arguments->head));
        ASTNode_set_temp_reg(node, ASTNode_get_temp_reg(node->funccall.arguments->head));
        return;
    }

    if (strcmp(node->funccall.name, "print_str") == 0)
    {
        EMIT1OP(PRINT, str_const(node->funccall.arguments->head->literal.string));
        ASTNode_set_temp_reg(node, empty_operand());
        return;
    }

    Operand store = virtual_register();

    int num_parms = node->funccall.arguments->size;
    Operand* regs = malloc(sizeof(Operand) * num_parms);
    int i = 0;


    // Evaluate args
    FOR_EACH(ASTNode*, arg, node->funccall.arguments)
    {
        ASTNode_copy_code(node, arg);
        regs[i] = ASTNode_get_temp_reg(arg);
        i++;
    }

    // Push args to stack
    for (int i = num_parms - 1; i >= 0; i--)
    {
        EMIT1OP(PUSH, regs[i]);
    }

    EMIT1OP(CALL, call_label(node->funcdecl.name));
    EMIT3OP(ADD_I, stack_register(), int_const(8 * num_parms), stack_register());
    EMIT2OP(I2I, return_register(), store);
    ASTNode_set_temp_reg(node, store); 

    free(regs);

    return;
}

void break_pre(NodeVisitor* visitor, ASTNode* node)
{
    return;
}

void break_post(NodeVisitor* visitor, ASTNode* node)
{
    EMIT1OP(JUMP, DATA->brk_label);

    return;
}

void cont_pre(NodeVisitor* visitor, ASTNode* node)
{
    return;
}

void cont_post(NodeVisitor* visitor, ASTNode* node)
{
    EMIT1OP(JUMP, DATA->cont_label);
    return;
}

InsnList* generate_code (ASTNode* tree)
{
    InsnList* iloc = InsnList_new();


    NodeVisitor* v = NodeVisitor_new();
    v->data = CodeGenData_new();
    v->dtor = (Destructor)CodeGenData_free;
    v->postvisit_program     = CodeGenVisitor_gen_program;
    v->previsit_funcdecl     = CodeGenVisitor_previsit_funcdecl;
    v->postvisit_funcdecl    = CodeGenVisitor_gen_funcdecl;
    v->previsit_vardecl      = vardecl_pre;
    v->postvisit_vardecl     = vardecl_post;
    v->previsit_block        = block_pre;
    v->postvisit_block       = block_post;
    v->previsit_assignment   = assignment_pre;
    v->postvisit_assignment  = assignment_post;
    v->previsit_conditional  = conditional_pre;
    v->postvisit_conditional = conditional_post;
    v->previsit_whileloop    = while_pre;
    v->postvisit_whileloop   = while_post;
    v->previsit_return       = return_pre;
    v->postvisit_return      = return_post;
    v->previsit_break        = break_pre;
    v->postvisit_break       = break_post;
    v->previsit_continue     = cont_pre;
    v->postvisit_continue    = cont_post;
    v->previsit_binaryop     = binop_pre;
    v->invisit_binaryop      = NULL;
    v->postvisit_binaryop    = binop_post;
    v->previsit_unaryop      = unop_pre;
    v->postvisit_unaryop     = unop_post;
    v->previsit_location     = location_pre;
    v->postvisit_location    = location_post;
    v->previsit_funccall     = funccall_pre;
    v->postvisit_funccall    = funccall_post;
    v->previsit_literal      = literal_pre;
    v->postvisit_literal     = literal_post;

    if (!tree)
    {
        return iloc;
    }

    /* generate code into AST attributes */
    NodeVisitor_traverse_and_free(v, tree);

    /* copy generated code into new list (the AST may be deallocated before
     * the ILOC code is needed) */
    FOR_EACH(ILOCInsn*, i, (InsnList*)ASTNode_get_attribute(tree, "code")) {
        InsnList_add(iloc, ILOCInsn_copy(i));
    }

    return iloc;
}
