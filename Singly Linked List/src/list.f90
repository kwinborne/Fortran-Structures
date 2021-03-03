!BSD 3-Clause License
!
!Copyright (c) 2021, Kiyle Winborne
!All rights reserved.
!
!Redistribution and use in source and binary forms, with or without
!modification, are permitted provided that the following conditions are met:
!
!1. Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
!2. Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
!3. Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
!THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
!AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
!IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
!FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
!DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
!SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
!CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
!OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
!OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.  
    
    
! Unit name: List_Module
! Author: Kiyle Winborne
! Date: 3/2/2021
!
! Dependencies: List_Node_Module
!
! Purpose: Defines a singly linked list data structure.
! Procedure, Type-bound: Append - Add a node to the end of the list.
!       Argument types: List, List_Node
!
! Procedure, Type-bound: get_next - Returns the next node in the list, starting from the head node.
!       Argument types: List, Integer
!  
! Procedure, Type-bound: free_list - Removes each node in the list and deallocates the memory.
!       Argument types: List   
!
! Procedure, Type-bound: Remove - Remove a node at any position in the list. 
!       Argument types: List, Integer
    
module List_Module
    use List_Node_Module
    implicit none
    
    !========================================================================
    !Definition of a singly linked list data structure.
    !Each list consists of a head node, defined in List_Node_Module
    !and a tail node. Structure also contains a list length for processing.
    !========================================================================
    type, public :: List
        integer :: length_of = 0
        type(List_Node), pointer :: head
        type(List_Node), pointer :: tail
    contains
        procedure, pass(this) :: append
        procedure, pass(this) :: remove_head
        procedure, pass(this) :: remove
        procedure, pass(this) :: remove_tail
        procedure, pass(this) :: free_list
        procedure, pass(this) :: get_next
    end type List
    
    contains
    !=============================
    !  Append a node to the list
    !=============================
    subroutine append (this, element)
        class(List), intent(in out) :: this
        class(*), intent(in) :: element
        if(associated (this%tail)) then
            allocate(this%tail%next, source = new_node(element, null()))
            this%tail => this%tail%next
            this%tail%List_Position = this%length_of+1
        else
            allocate(this%head, source = new_node(element, null()))
            this%tail => this%head
            this%head%list_position = this%length_of +1
        end if
        this%length_of = this%length_of + 1
    end subroutine append
    !================================
    ! Remove all nodes from the list
    !================================
    subroutine free_list (this)
        class(List), intent(inout) :: this
        do while(this%length_of /= 0)
            call this%remove_tail()
        end do
    end subroutine free_list
    !===================================
    ! Return the next node in the list
    !===================================
    function get_next(this, node_position) result (node)
        class(List), intent(in) :: this
        integer, intent(in) :: node_position
        type(List_Node) :: node
        type(List_Node), pointer :: current_node
        integer :: i
        current_node => this%head
        do i = 2, node_position
        current_node => current_node%next
            end do
            node = current_node
    end function get_next
    
    !===================================
    ! Remove a single node from the list
    !===================================
    subroutine remove (this, node_position)
        class(List), intent(inout) :: this
        integer, intent(in) :: node_position
        type(List_Node) :: node
        type(List_Node), pointer :: current_node
        type(List_Node), pointer :: temp_node
        integer :: i
        current_node => this%head
        if(node_position == 1) then
            call this%remove_head()
            return
        else if(node_position == this%length_of) then
            call this%remove_tail()
            return
        end if
        do i = 2, node_position
            if(i == node_position) then
                temp_node => current_node
            end if
        current_node => current_node%next
        end do
        temp_node%next => current_node%next
        deallocate(current_node%element)
        current_node%next => null()
        this%length_of = this%length_of - 1
    end subroutine remove
    
        subroutine remove_head (this)
        class(list), intent(inout) :: this
        type(List_Node) :: node_to_remove
        node_to_remove = this%head
        this%head = this%head%next
        deallocate(node_to_remove%element)
        node_to_remove%next => null()
        this%length_of = this%length_of -1
    end subroutine remove_head
    
    
    subroutine remove_tail (this)
        class(list), intent(inout) :: this
        type(List_Node) :: node_to_remove
        node_to_remove = this%tail
        deallocate(node_to_remove%element)
        this%length_of = this%length_of - 1
    end subroutine remove_tail
    

end module List_Module