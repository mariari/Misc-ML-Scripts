(* TODO ∷ PROPOSALS
 * Have a choice option, that allows you to pick alternative strategies.
 * _For example we can have_
 * - Strategy A - Simply, veritably correct
 * - Strategy B - Harder, more Complex
 * And then test the compiler with these settings

 * Have optional passes as a construct
 * This would allow the backend to chose if it wants a pass. This us more useful
 * as we go down the compiler pipeline.

 * Being able to denote dependencies of passes would be great as well
 * For example the if-to-case pass relies upon cond-to-if, and so being able to denote
 * that in some way in the compiler would be great!

 * Add Gensym to Meta.t or to the Pipeline.surrounding_environment, if we add it to the
 * later, than maybe the return type should also attach the volatile part of the
 * Pipeline.surrounding_environment
 *)

(********************************************************************************)
(*** Modules that belong to different files *)
(********************************************************************************)
module Feedback : sig
  type t
end

module Trace : sig
  type t
end

module Context : sig
  type 'a t
end

module Sexp : sig
  type t
end

module NameSymbol : sig
  type t
end

(********************************************************************************)
(** Meta Information and computationally relevant information *)
(********************************************************************************)

module Meta : sig
  type t = { feedback : Feedback.t; trace : Trace.t }
end

module ComputationResult : sig
  type 'a result =
    | Failure
    | Partial of 'a
    | Success of 'a

  type 'a t = { meta : Meta.t; res : 'a result}
end

(********************************************************************************)
(** Pipeline *)
(********************************************************************************)

(** A Recursive list represents the concept of a list with a chance to nest
 * This allows us to get a tree like structure,
 * where A pipeline step can contain more steps
 *)
module RecursiveList : sig
  type 'a t
end

(** A circular list represents the idea of steps that can be recursive
 * on each other
 *)
module CircularList : sig
  (* TODO : add a recursion function to the recursive bit, that tells
   * it how it's recursive. perhaps it uses a predicate function
   * on the 'a type to determine what function to go to next.
   *)
  type 'a recursive_schema =
    | Recursive of 'a list
    | NonRecursive of 'a

  type 'a t =
    'a recursive_schema RecursiveList.t
end

module Pipeline : sig

  (** [env_or_sexp] expresses data that may be already added to the environment
   * or are still in a base sexp form that requires no lookup
   *)
  type env_or_sexp =
    | InEnv of NameSymbol.t
    | Sexp  of Sexp.t

  (* Extension: Every Pass *)
  (** [around] represents when a pass modifier is run before each pass
   * runs on a singular [env_or_sexp] *)
  type around = Before | After

  (** [working_environment] serves as the input data relevant to any
   * pipeline step function. This is what is actively being processed
   * in the pipeline itself.
   *)
  type working_environment = {
    current_expression : env_or_sexp list;
    context            : Sexp.t Context.t
  }

  (** [surrounding_environment] serves as the minimum surrounding information
   * in the environment of the Pipeline Steps.
   *)
  type surrounding_environment = {
    (* This will be taken as a read value *)
    (** The [current_step_name] represents the current running step name *)
    current_step_name : NameSymbol.t option;
    meta_information  : Meta.t;
    (* Extension: Every Pass *)
    (** denotes the list of functions that ought to be run before
        or after every pass on a single [env_or_sexpq]*)
    (* on_single_pass : (around * (Automation.pass_argument -> Automation.pass_argument)) list *)
  }

  (** [computational_input] is the actual data passed into the [Step.t] type.
   * The [language_data] field serves as the direct data that the direct data
   * a pass wishes to take, while [surrounding_data] serves as extra environment
   * constraints that one wants to include.
   *)
  type computational_input = {
    language_data    : working_environment;
    surrounding_data : surrounding_environment;
  }

  module Step : sig
    (** the pipeline step, takes the entire [computational_input] as input.
     * This is quite a wide type in that it has data that a pass does not
     * directly want to handle.
     * Thus it is expected one works with the [Automation] module, to make
     * fulfilling both the [ComputationResult.t] and the input form bearable.
     *)
    (* May promote [working_environment] to [information_record] *)
    type t = computational_input -> working_environment ComputationResult.t

    type named = {
      name : NameSymbol.t;
      func : t
    }

    val name_pass : t -> NameSymbol.t -> named

    val name_of_pass : named -> NameSymbol.t
  end

  module Environment : sig
    (* NOTE ∷ replace the stopping-step and related functions with the stop adt
     * Andy also mentioned a type class approach which may make it more dynamic,
     * consider that for the next representation instead of this ADT notion
     *)
    type stop_adt = Stop

    type t = {
      (** [information] serves as the public facing data to the environment
       * that passes can access
       *)
      information : computational_input;

      (* All other information is made to be exclusive with the Pipeline as
       * a whole that the Steps do not bother with.
       * This consists of information like how to deal with Traces between passes
       * to the entire pass infrastructure itself.
       *)

      (** [registered_pipeline] is the pipeline itself.
       * It features all the steps named and registered with the system *)
      registered_pipeline : Step.named CircularList.t;

      stopping_step : NameSymbol.t option
    }

    (** [register_step] registers the pipeline function to the environment *)
    val register_step : Step.named CircularList.t -> unit

    (** [def_pipline_group] creates a named group of pipeline steps or a
     * nested grouping of pipeline steps.
     *)
    val def_pipline_group
        : NameSymbol.t
          -> Step.named CircularList.t list
          -> Step.named CircularList.t

    (** [stop_at] tells the environment to stop at a particular step when
     * running the environment.
     *)
    val stop_at : NameSymbol.t -> unit

    (** [stop_at_nothing] tells the environment to run the compiler fully.
     *)
    val stop_at_nothing : unit

    (** [eval] is responsible for taking the environment, running it
     * to the desired  point and giving back what data is left.
     * The output type should be more carefully considered
     * and have many operations on it.
     *)
    val eval : t -> computational_input

    (** [run] serves as the running point, the unit argument is a placeholder
     * for the monadic functions used to build up the environment
     * the [t] is the given pre-built environment
     *)
    val run : unit -> t -> computational_input

    val extract : unit -> t
  end
end

(********************************************************************************)
(** Automated Tooling *)
(********************************************************************************)

(** the job of [Automation] is to make the step function more amenable to writing passes
 * It is not ergonomic to take extra information one may not care about *)
module Automation : sig
  type stage
    = Current
    | FromTopToCurrent
    | Eval

  type process_job = {
    current   : Pipeline.env_or_sexp;
    new_forms : (stage * Pipeline.env_or_sexp) list
  }

  (** [process_job_no_env] is like [process_job] but it does not contain
   * any reference to names that might be stored in the [Context.t]
   *)
  type process_job_no_env = {
    current : Sexp.t;
    new_forms : (stage * Sexp.t) list
  }

  type t =
    (** [ProcessJob] can not update the context, so the data given
     * can't be a name referenced in the Context
     *)
    | ProcessJob of process_job_no_env
    | UpdateJob  of { new_context : Sexp.t Context.t;
                      process : process_job
                    }

  type job = t

  (** [pass_arguments] is a pipeline processing function, namely we wrap
   * the current expression and the context into a single function *)
  type pass_argument = {
    current : Pipeline.env_or_sexp;
    context : Sexp.t Context.t
  }

  (** [simplified_pass_argument] represents the simplified version of
   * [pass_argument] for whose passes simply don't care about where
   * the sexp comes from.
   *)
  type simplified_pass_argument = {
    current : Sexp.t;
    context : Sexp.t Context.t
  }

  (* This represents the monad type that we require for a pass *)
  module type MonadEff = sig
    type 'a t
    type 'a output_eff
    (* Trace State effect *)
    val get_trace : unit    -> Trace.t t
    val set_trace : Trace.t -> unit    t
    (* feed_back State effect *)
    val get_feedback : unit       -> Feedback.t t
    val set_feedback : Feedback.t -> unit   t
    (* combining the two effects above  *)
    val get_meta : unit   -> Meta.t t
    val set_meta : Meta.t -> unit t

    (* This is in all likelihood what is needed to setup the environment
     * usurping the above effects for actually setting up the pass
     * we leave the above effects for demonstrative purposes
     *)
    val has_env   : unit -> Pipeline.surrounding_environment t
    val set_env   : Pipeline.surrounding_environment -> unit t
    (* Comonad effect *)
    val run      : 'a t -> 'a output_eff
  end

  val transform_output_type
      : job list -> Pipeline.working_environment

  (* Type class in Haskell that dictates being able to extract to a step *)
  module Runable :
    functor (M : MonadEff) -> sig
      type 'a t = 'a M.t

      (** [run] runs the given environment, extracting out a result with meta data
       * over the resulting value.
       *)
      val run : 'a t -> 'a ComputationResult.t M.output_eff

      (** [apply_simplified_pass] serves as a HOF that allows for passes to be
       * a more simplified type, namely a function that takes a [pass_arugment]
       * to an effectual result over [job] that determines how the pass should
       * be brought together.
       * Note that [Pipeline.computational_input -> Pipeline.working_environment t]
       * is an approximation of the [Step.t] type without the Meta information attached
       *)
      val apply_simplified_pass
          : (pass_argument -> job t)
            -> Pipeline.computational_input
            -> Pipeline.working_environment t

      (** [run_simplified_pass] simply combines the [run] function with
       * [apply_simplified_pass], to get the output effect, which corresponds to a
       * [Pipeline.Step.t] with an effect attached to it
       *)
      val run_simplified_pass
          : (pass_argument -> job M.output_eff)
            -> Pipeline.computational_input
            -> Pipeline.working_environment ComputationResult.t M.output_eff

      (** [simplify] allows a pass to ignore the fact that expression coming in may
       * be added to the [Context.t] already, and we can act as if it were just a
       * normal [Sexp.t] being passed in.
       *)
      val simplify : (simplified_pass_argument -> job M.output_eff)
                     -> (pass_argument -> job M.output_eff)

  end
end


(* Runnable and MonadEff translated into Haskell *)
(*
  type class HasExtract a m | a -> m where
     extract :: a x -> m (ComputationResult.t x)

  data Automation.PassArgument
    = PassArg { context :: Context.t Sexp.t, current :: Pipeline.env_or_sexp }

  transformOutputType :: Automation.t -> Pipeline.workingEnvironment

  -- we share the one m monad here for the function
  -- because we will extract it after we run the m effect
  applySimplifiedPass
    :: (HasMeta m)
    => (Automation.PassArgument -> m Automation.t)
    -- These form Pipeline.Step.t for some m over the
    -- output modulo the ComputationResult.t over it
    -> Pipeline.ComputationalInput
    -> m Pipeline.WorkingEnvironment

  runSimplifiedPass
    :: (HasExtract _a m)
    => (Automation.PassArgument -> m Automation.t)
    -> Pipeline.ComputationalInput
    -> m (ComputationResult.t Pipeline.WorkingEnvironment)
  runSimplifiedPass f = do
    extract . applySimplifiedPass f

 *)
(* type t = computational_input -> working_environment ComputationResult.t *)
