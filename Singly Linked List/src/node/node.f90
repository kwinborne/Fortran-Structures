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
    
    
    Unit name: List_Node_Module
! Author: Kiyle Winborne
! Date: 3/2/2021
!
! Dependencies: None
!
! Purpose: Defines a node data type for use in a linked list.
! Each node contains a data element and a pointer to the next node.
!
! Procedure, Module : new_node - Creates a new node.
! Interface: get_element : Module Procedures get_(data-type)
    
 module List_Node_Module
    implicit none
    type, public :: List_Node
        class(*), allocatable :: element
        type(List_Node), pointer :: next => null()
        integer :: list_position
    end type List_Node
    
    interface get_element
        module procedure get_char_element
        module procedure get_int_element
        module procedure get_real_element
        module procedure get_complex_element
        module procedure get_logical_element
    end interface get_element
    
    
    contains
    
    function new_node(element, next) result(node)
    class(*), intent(in) :: element
    type(List_Node), pointer :: next
    type(List_Node) :: node
        allocate(node%element, source = element)
        node%next => next
    end function new_node
    
    
    
     subroutine get_char_element(this, element_character)
        type(List_Node), intent(in) :: this
        character(*), intent(out) :: element_character
        integer :: status
        if(allocated(this%element)) then
            select type (element => this%element)
            type is (character(*))
                element_character = element
                status = 0
            class default
                status = -1
            end select
        else
                status = -2
        end if
                
    end subroutine get_char_element
    
    subroutine get_int_element(this, element_integer)
        type(List_Node),intent(in)::this
        integer, intent(out) :: element_integer
            if(allocated(this%element)) then
            select type (element => this%element)
            type is (integer)
                element_integer = element
            class default
                element_integer = 0
            end select
        end if
    end subroutine get_int_element
    
    subroutine get_real_element(this, element_real)
        type(List_Node), intent(in) :: this
        real, intent(out) :: element_real
        integer :: status
            if(allocated(this%element)) then
            select type (element => this%element)
            type is (real)
                element_real = element

            end select
        end if
    end subroutine get_real_element
    
    subroutine get_complex_element(this, element_complex)
        type(List_Node), intent(in) :: this
        complex, intent(out) :: element_complex
        integer :: status
            if(allocated(this%element)) then
            select type (element => this%element)
            type is (complex)
                element_complex = element
                status = 0
            class default
                status = -1
            end select
            else
                status = -2
            end if     
    end subroutine get_complex_element
    
    subroutine get_logical_element(this, element_logical)
        type(List_Node), intent(in) :: this
        logical, intent(out) :: element_logical
        integer :: status
            if(allocated(this%element)) then
            select type (element => this%element)
            type is (logical)
                element_logical = element
                status = 0
            class default
                status = -1
            end select
            else
                status = -2
            end if
        
    end subroutine get_logical_element
    
end module List_Node_Module
